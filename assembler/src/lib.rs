use std::{
    fmt::{Debug, Display},
    fs::File,
    hash::Hash,
    iter::once,
    ops::Range,
    path::Path,
};

use ariadne::{Cache, Color, Label, Report};
use asm_ir::{Ast, Program};
use cache::{CacheStr, FsCache};
use error::error_as_reports;
use parser::{lines, Addr, Arg, ConstantAddr, Span};

use crate::parser::Line;

pub mod cache;
#[cfg(feature = "cli")]
pub mod config;
pub mod error;
mod from_str_radix;
pub mod parser;

#[cfg(feature = "cli")]
pub fn parse<P: AsRef<Path>>(input: &[P], output: P) {
    // TODO remove unwraps
    let mut cache = FsCache::new();
    for p in input {
        cache.add(p)
    }
    let program = if input.len() == 1 {
        let i = &input[0];
        let v = match parse_file(&i.as_ref().to_path_buf(), &mut cache) {
            Err(ParseErr::Reports(r)) => {
                for report in r {
                    report.eprint(&mut cache).unwrap();
                }
                return; // Do not continue
            }
            Err(ParseErr::FileError(e)) => {
                eprintln!("File error: {}", e);
                return; // Do not continue
            }
            Ok(v) => v,
        };

        let mut p = Program::new();
        p.segments.push((0, v));
        p
    } else {
        todo!()
    };

    let mut f = File::create(output).unwrap();
    program.write(&mut f).unwrap();
}

pub enum ParseErr<Id: std::fmt::Debug + Hash + Eq + Clone, E> {
    Reports(Vec<Report<(Id, Range<usize>)>>),
    FileError(E),
}

impl<Id: std::fmt::Debug + Hash + Eq + Clone, E> From<Vec<Report<(Id, Range<usize>)>>>
    for ParseErr<Id, E>
{
    fn from(v: Vec<Report<(Id, Range<usize>)>>) -> Self {
        Self::Reports(v)
    }
}

impl<Id: std::fmt::Debug + Hash + Eq + Clone, E> From<Report<(Id, Range<usize>)>>
    for ParseErr<Id, E>
{
    fn from(v: Report<(Id, Range<usize>)>) -> Self {
        Self::Reports(vec![v])
    }
}

pub fn parse_file<'a, Id, C, E>(id: &'a Id, cache: &'a mut C) -> Result<Vec<Ast>, ParseErr<Id, E>>
where
    Id: std::fmt::Debug + Hash + Eq + Clone,
    C: Cache<Id> + CacheStr<Id, Error = E> + 'a,
{
    let s = match cache.get_str(id) {
        Ok(v) => v.chars().collect::<String>(),
        Err(e) => return Err(ParseErr::FileError(e)),
    };
    let (rest, instrs) = lines(Span::new(&s)).map_err(|e| error_as_reports(e, id))?;
    // println!("Instrs: {:?}", instrs);
    if !rest.is_empty() {
        panic!(
            "ICE: Unexpected rest when parsing lines: {}",
            rest.fragment()
        )
    }
    let mut res = Vec::with_capacity(instrs.len());
    let mut reports = Vec::new();
    for i in instrs {
        match i.extra {
            Line::Instr((opcode, args)) => {
                // println!("Parsing {} {}", opcode, args.iter().map(|x| ArgKind::from(&x.extra).to_string()).collect::<Vec<_>>().join(", "));
                match *opcode.fragment() {
                    "NOP" => {
                        match expected_args_whole(id.clone(), &opcode, &args, &[&[]]) {
                            Ok(_) => res.push(Ast::Nop),
                            Err(e) => reports.extend(e),
                        };
                    }
                    "LD" => {
                        match expected_args_whole(
                            id.clone(),
                            &opcode,
                            &args,
                            &[
                                &[ArgKind::Register, ArgKind::Byte],
                                &[ArgKind::Register, ArgKind::Register],
                                &[ArgKind::AddrPointer, ArgKind::Register],
                                &[ArgKind::Register, ArgKind::AddrPointer],
                                &[ArgKind::Pointer, ArgKind::ConstantAddr],
                                &[ArgKind::Pointer, ArgKind::Register, ArgKind::ConstantAddr],
                            ],
                        ) {
                            Ok(x) => res.push(match (x, args[0].extra, args[1].extra) {
                                (0, Arg::Register(x), Arg::Byte(b)) => Ast::LoadByte(x, b),
                                (1, Arg::Register(x), Arg::Register(y)) => Ast::LoadReg(x, y),
                                (2, Arg::Addr(Addr::Pointer), Arg::Register(y)) => {
                                    Ast::LoadFromRegs(y)
                                }
                                (3, Arg::Register(y), Arg::Addr(Addr::Pointer)) => {
                                    Ast::LoadIntoRegs(y)
                                }
                                (
                                    4,
                                    Arg::ConstantAddr(ConstantAddr::Pointer),
                                    Arg::ConstantAddr(ConstantAddr::Addr(x)),
                                ) => Ast::LoadPointer(x),
                                (5, Arg::ConstantAddr(ConstantAddr::Pointer), Arg::Register(x)) => {
                                    match args[2].extra {
                                        Arg::ConstantAddr(ConstantAddr::Addr(y)) => {
                                            Ast::LoadPointerOffset(x, y)
                                        }
                                        _ => unreachable!(),
                                    }
                                }
                                _ => unreachable!(),
                            }),
                            Err(e) => reports.extend(e),
                        };
                    }
                    "RET" => {
                        match expected_args_whole(id.clone(), &opcode, &args, &[&[]]) {
                            Ok(_) => res.push(Ast::Return),
                            Err(e) => reports.extend(e),
                        };
                    }
                    "CALL" => {
                        match expected_args_whole(
                            id.clone(),
                            &opcode,
                            &args,
                            &[
                                &[ArgKind::ConstantAddr],
                                &[ArgKind::Register, ArgKind::ConstantAddr],
                                &[ArgKind::Pointer],
                            ],
                        ) {
                            Ok(x) => {
                                res.push(match (x, args[0].extra, args.get(1).map(|x| x.extra)) {
                                    (0, Arg::ConstantAddr(ConstantAddr::Addr(x)), None) => {
                                        Ast::Call(x)
                                    }
                                    (
                                        1,
                                        Arg::Register(x),
                                        Some(Arg::ConstantAddr(ConstantAddr::Addr(y))),
                                    ) => Ast::CallOffset(x, y),
                                    (2, Arg::ConstantAddr(ConstantAddr::Pointer), None) => {
                                        Ast::CallPointer
                                    }
                                    _ => unreachable!(),
                                })
                            }
                            Err(e) => reports.extend(e),
                        };
                    }
                    "JP" => {
                        match expected_args_whole(
                            id.clone(),
                            &opcode,
                            &args,
                            &[
                                &[ArgKind::ConstantAddr],
                                &[ArgKind::Register, ArgKind::ConstantAddr],
                                &[ArgKind::Pointer],
                            ],
                        ) {
                            Ok(x) => {
                                res.push(match (x, args[0].extra, args.get(1).map(|x| x.extra)) {
                                    (0, Arg::ConstantAddr(ConstantAddr::Addr(x)), None) => {
                                        Ast::Jump(x)
                                    }
                                    (
                                        1,
                                        Arg::Register(x),
                                        Some(Arg::ConstantAddr(ConstantAddr::Addr(y))),
                                    ) => Ast::JumpOffset(x, y),
                                    (2, Arg::ConstantAddr(ConstantAddr::Pointer), None) => {
                                        Ast::JumpPointer
                                    }
                                    _ => unreachable!(),
                                })
                            }
                            Err(e) => reports.extend(e),
                        };
                    }
                    "ADD" => {
                        match expected_args_whole(
                            id.clone(),
                            &opcode,
                            &args,
                            &[
                                &[ArgKind::Register, ArgKind::Byte],
                                &[ArgKind::Register, ArgKind::Register],
                                &[ArgKind::Pointer, ArgKind::Register],
                            ],
                        ) {
                            Ok(x) => res.push(match (x, args[0].extra, args[1].extra) {
                                (0, Arg::Register(x), Arg::Byte(b)) => Ast::AddByte(x, b),
                                (1, Arg::Register(x), Arg::Register(y)) => Ast::AddReg(x, y),
                                (2, Arg::ConstantAddr(ConstantAddr::Pointer), Arg::Register(y)) => {
                                    Ast::AddToPointer(y)
                                }
                                _ => unreachable!(),
                            }),
                            Err(e) => reports.extend(e),
                        };
                    }
                    "SUB" => {
                        match expected_args_whole(
                            id.clone(),
                            &opcode,
                            &args,
                            &[&[ArgKind::Register, ArgKind::Register]],
                        ) {
                            Ok(x) => res.push(match (x, args[0].extra, args[1].extra) {
                                (0, Arg::Register(x), Arg::Register(y)) => Ast::Sub(x, y),
                                _ => unreachable!(),
                            }),
                            Err(e) => reports.extend(e),
                        };
                    }
                    "SUBN" => {
                        match expected_args_whole(
                            id.clone(),
                            &opcode,
                            &args,
                            &[&[ArgKind::Register, ArgKind::Register]],
                        ) {
                            Ok(x) => res.push(match (x, args[0].extra, args[1].extra) {
                                (0, Arg::Register(x), Arg::Register(y)) => Ast::SubNeg(x, y),
                                _ => unreachable!(),
                            }),
                            Err(e) => reports.extend(e),
                        };
                    }
                    "OR" => {
                        match expected_args_whole(
                            id.clone(),
                            &opcode,
                            &args,
                            &[&[ArgKind::Register, ArgKind::Register]],
                        ) {
                            Ok(x) => res.push(match (x, args[0].extra, args[1].extra) {
                                (0, Arg::Register(x), Arg::Register(y)) => Ast::Or(x, y),
                                _ => unreachable!(),
                            }),
                            Err(e) => reports.extend(e),
                        };
                    }
                    "XOR" => {
                        match expected_args_whole(
                            id.clone(),
                            &opcode,
                            &args,
                            &[&[ArgKind::Register, ArgKind::Register]],
                        ) {
                            Ok(x) => res.push(match (x, args[0].extra, args[1].extra) {
                                (0, Arg::Register(x), Arg::Register(y)) => Ast::Xor(x, y),
                                _ => unreachable!(),
                            }),
                            Err(e) => reports.extend(e),
                        };
                    }
                    "AND" => {
                        match expected_args_whole(
                            id.clone(),
                            &opcode,
                            &args,
                            &[&[ArgKind::Register, ArgKind::Register]],
                        ) {
                            Ok(x) => res.push(match (x, args[0].extra, args[1].extra) {
                                (0, Arg::Register(x), Arg::Register(y)) => Ast::And(x, y),
                                _ => unreachable!(),
                            }),
                            Err(e) => reports.extend(e),
                        };
                    }
                    "SE" => {
                        match expected_args_whole(
                            id.clone(),
                            &opcode,
                            &args,
                            &[
                                &[ArgKind::Register, ArgKind::Byte],
                                &[ArgKind::Register, ArgKind::Register],
                            ],
                        ) {
                            Ok(x) => res.push(match (x, args[0].extra, args[1].extra) {
                                (0, Arg::Register(x), Arg::Byte(b)) => Ast::SkipEqByte(x, b),
                                (1, Arg::Register(x), Arg::Register(y)) => Ast::SkipEqReg(x, y),
                                _ => unreachable!(),
                            }),
                            Err(e) => reports.extend(e),
                        };
                    }
                    "SNE" => {
                        match expected_args_whole(
                            id.clone(),
                            &opcode,
                            &args,
                            &[
                                &[ArgKind::Register, ArgKind::Byte],
                                &[ArgKind::Register, ArgKind::Register],
                            ],
                        ) {
                            Ok(x) => res.push(match (x, args[0].extra, args[1].extra) {
                                (0, Arg::Register(x), Arg::Byte(b)) => Ast::SkipNotEqByte(x, b),
                                (1, Arg::Register(x), Arg::Register(y)) => Ast::SkipNotEqReg(x, y),
                                _ => unreachable!(),
                            }),
                            Err(e) => reports.extend(e),
                        };
                    }
                    "SHR" => {
                        match expected_args_whole(
                            id.clone(),
                            &opcode,
                            &args,
                            &[&[ArgKind::Register]],
                        ) {
                            Ok(x) => res.push(match (x, args[0].extra) {
                                (0, Arg::Register(x)) => Ast::ShiftRight(x),
                                _ => unreachable!(),
                            }),
                            Err(e) => reports.extend(e),
                        };
                    }
                    "SHL" => {
                        match expected_args_whole(
                            id.clone(),
                            &opcode,
                            &args,
                            &[&[ArgKind::Register]],
                        ) {
                            Ok(x) => res.push(match (x, args[0].extra) {
                                (0, Arg::Register(x)) => Ast::ShiftLeft(x),
                                _ => unreachable!(),
                            }),
                            Err(e) => reports.extend(e),
                        };
                    }
                    x => unreachable!("Unexpected opcode: {}", x),
                }
            }
            Line::Label(label) => {
                todo!("Symbol management code not written (Label: {})", label)
            }
        }
    }
    println!("{} reports", reports.len());
    if !reports.is_empty() {
        return Err(reports.into());
    }
    Ok(res)
}

enum ArgKind {
    Byte,
    Register,
    AddrPointer,
    Addr,
    AddrSymbol,
    Pointer,
    ConstantAddr,
    ConstantSymbol,
}

impl<'a> PartialEq<Arg<'a>> for ArgKind {
    fn eq(&self, other: &Arg) -> bool {
        matches!(
            (self, other),
            (ArgKind::Byte, Arg::Byte(_))
                | (ArgKind::Register, Arg::Register(_))
                | (ArgKind::AddrPointer, Arg::Addr(Addr::Pointer))
                | (ArgKind::Addr, Arg::Addr(Addr::Addr(_)))
                | (ArgKind::Pointer, Arg::ConstantAddr(ConstantAddr::Pointer))
                | (
                    ArgKind::ConstantAddr,
                    Arg::ConstantAddr(ConstantAddr::Addr(_))
                )
        )
    }
}

impl Display for ArgKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArgKind::Byte => write!(f, "byte"),
            ArgKind::Register => write!(f, "register"),
            ArgKind::AddrPointer => write!(f, "[I]"),
            ArgKind::Addr => write!(f, "[address]"),
            ArgKind::AddrSymbol => write!(f, "[symbol]"),
            ArgKind::Pointer => write!(f, "I"),
            ArgKind::ConstantAddr => write!(f, "#address"),
            ArgKind::ConstantSymbol => write!(f, "symbol"),
        }
    }
}

impl Debug for ArgKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl<'a> From<&Arg<'a>> for ArgKind {
    fn from(a: &Arg) -> Self {
        match a {
            Arg::Addr(Addr::Addr(_)) => Self::Addr,
            Arg::Addr(Addr::Pointer) => Self::AddrPointer,
            Arg::Addr(Addr::Symbol(_)) => Self::AddrSymbol,
            Arg::Byte(_) => Self::Byte,
            Arg::Register(_) => Self::Register,
            Arg::ConstantAddr(ConstantAddr::Pointer) => Self::Pointer,
            Arg::ConstantAddr(ConstantAddr::Addr(_)) => Self::ConstantAddr,
            Arg::ConstantAddr(ConstantAddr::Symbol(_)) => Self::ConstantSymbol,
        }
    }
}

fn expected_args_single<Id: std::fmt::Debug + Hash + Eq + Clone>(
    id: Id,
    args: &[Span<Arg>],
    kind: &[ArgKind],
) -> Vec<Report<(Id, Range<usize>)>> {
    let mut r = vec![];
    for (arg, k) in args.iter().zip(kind.iter().map(Some).chain(once(None))) {
        if let Some(k) = k {
            if k != &arg.extra {
                r.push(
                    Report::build(
                        ariadne::ReportKind::Error,
                        id.clone(),
                        arg.location_offset(),
                    )
                    .with_message("Unexpected argument type")
                    .with_label(
                        Label::new((
                            id.clone(),
                            arg.location_offset()..(arg.location_offset() + arg.fragment().len()),
                        ))
                        .with_message(format!(
                            "Expected {} but found {:?} ({})",
                            k,
                            arg.fragment(),
                            ArgKind::from(&arg.extra)
                        ))
                        .with_color(Color::Red),
                    )
                    .finish(),
                );
            }
        } else {
            r.push(
                Report::build(
                    ariadne::ReportKind::Error,
                    id.clone(),
                    arg.location_offset(),
                )
                .with_message("Syntax error")
                .with_label(
                    Label::new((
                        id,
                        arg.location_offset()..(arg.location_offset() + arg.fragment().len()),
                    ))
                    .with_message("Unexpected argument")
                    .with_color(Color::Red),
                )
                .with_note(format!(
                    "Expected {} arguments, but found {}",
                    kind.len(),
                    args.len()
                ))
                .finish(),
            );
            break;
        }
    }
    r
}

#[allow(clippy::type_complexity)]
fn expected_args_whole<Id: std::fmt::Debug + Hash + Eq + Clone>(
    id: Id,
    opcode: &Span,
    args: &[Span<Arg>],
    kind: &[&[ArgKind]],
) -> Result<usize, Vec<Report<(Id, Range<usize>)>>> {
    if kind.is_empty() {
        panic!("can't check for nothing")
    } else if kind.len() == 1 {
        let r = expected_args_single(id, args, kind[0]);
        if r.is_empty() {
            Ok(0)
        } else {
            Err(r)
        }
    } else {
        let mut idx = None;
        let mut matches = vec![true; args.len()];
        for (i, arg_kinds) in kind.iter().enumerate() {
            if args
                .iter()
                .zip(arg_kinds.iter().map(Some).chain(once(None)))
                .enumerate()
                .all(|(i, (arg, kind))| {
                    let m = kind.map(|x| x == &arg.extra).unwrap_or(false);
                    // println!("{}: {:?} == {} = {}", i, kind, arg, m);
                    matches[i] &= m;
                    m
                })
            {
                idx = Some(i)
            }
        }
        if let Some(idx) = idx {
            Ok(idx)
        } else if args.is_empty() {
            let pos = opcode.location_offset() + opcode.fragment().len();
            Err(vec![Report::build(
                ariadne::ReportKind::Error,
                id.clone(),
                pos,
            )
            .with_message("Unexpected arguments")
            .with_label(
                Label::new((id, pos..(pos + 1)))
                    .with_message(if kind.len() == 1 {
                        format!(
                            "expected {{{}}}",
                            kind[0]
                                .iter()
                                .skip(1)
                                .fold(format!("{}", kind[0][0]), |a, x| format!("{}, {}", a, x))
                        )
                    } else {
                        format!(
                            "expected one of {}",
                            kind.iter()
                                .map(|kind| format!(
                                    "{{{}}}",
                                    kind.iter().skip(1).fold(
                                        format!("{}", kind[0]),
                                        |a, x| format!("{}, {}", a, x)
                                    )
                                ))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    })
                    .with_color(Color::Red),
            )
            .finish()])
        } else {
            let start_idx = matches
                .iter()
                .enumerate()
                .find(|(_, x)| !**x)
                .map(|(i, _)| i)
                .unwrap();
            let start = args[start_idx];
            let end = args.last().unwrap();
            let range = start.location_offset()..(end.location_offset() + end.fragment().len());
            Err(vec![Report::build(
                ariadne::ReportKind::Error,
                id.clone(),
                start.location_offset(),
            )
            .with_message("Unexpected arguments")
            .with_label(
                Label::new((id, range))
                    .with_message(if kind.len() == 1 {
                        format!(
                            "expected {{{}}}",
                            kind[0].iter().skip(start_idx + 1).fold(
                                format!("{}", kind[0][start_idx]),
                                |a, x| format!("{}, {}", a, x)
                            )
                        )
                    } else {
                        format!(
                            "expected one of {}",
                            kind.iter()
                                .map(|kind| format!(
                                    "{{{}}}",
                                    kind.iter().skip(start_idx + 1).fold(
                                        format!("{}", kind[start_idx]),
                                        |a, x| format!("{}, {}", a, x)
                                    )
                                ))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    })
                    .with_color(Color::Red),
            )
            .finish()])
        }
    }
}
