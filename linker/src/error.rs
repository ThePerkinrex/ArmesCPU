use std::fmt::Display;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ProgramLinkError {
    SymbolNotDefined(String, Option<String>),
    SymbolNotDeclared(String, Option<String>),
    /// Symbol, section, addr
    SegmentNotFound(String, u16),
    SegmentAddrNotFound(String, u16, u16),
}

impl Display for ProgramLinkError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SymbolNotDefined(symbol, Some(file)) => write!(
                f,
                "{symbol:?} is not defined, i.e., it points to nowhere ({file})"
            ),
            Self::SymbolNotDefined(symbol, None) => {
                write!(f, "{symbol:?} is not defined, i.e., it points to nowhere ")
            }
            Self::SymbolNotDeclared(symbol, Some(file)) => {
                write!(f, "{symbol:?} is not declared ({file})")
            }
            Self::SymbolNotDeclared(symbol, None) => {
                write!(f, "{symbol:?} is not declared")
            }
            Self::SegmentNotFound(file, seg) => {
                write!(f, "segment {seg} not found in file {file}")
            }
            Self::SegmentAddrNotFound(file, seg, addr) => write!(
                f,
                "addr {addr:0>4x} not found @ segment {seg} in file {file}"
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ElfError {
    #[error(transparent)]
    ProgramLinkError(ProgramLinkError),
    /// Symbol, og file, dup file
    DuplicateSymbol(String, String, String),
}

impl Display for ElfError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ProgramLinkError(e) => e.fmt(f),
            Self::DuplicateSymbol(symbol, og, dup) => write!(
                f,
                "symbol {symbol:?} duplicate found in {dup} (original in {og})"
            ),
        }
    }
}

impl From<ProgramLinkError> for ElfError {
    fn from(e: ProgramLinkError) -> Self {
        Self::ProgramLinkError(e)
    }
}
