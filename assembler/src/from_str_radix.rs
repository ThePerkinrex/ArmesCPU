use std::{
    fmt::Display,
    num::{IntErrorKind, ParseIntError},
};

#[derive(Debug)]
pub struct FromStrRadixError {
    pub error: ParseIntError,
    name: &'static str,
    radix: u32,
}

impl std::error::Error for FromStrRadixError {}

impl FromStrRadixError {
    pub const fn name(&self) -> &'static str {
        self.name
    }

    pub const fn radix(&self) -> &u32 {
        &self.radix
    }
}

impl Display for FromStrRadixError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.error.kind() {
            IntErrorKind::Empty => write!(f, "cannot parse {} from empty string", self.name),
            IntErrorKind::InvalidDigit => write!(f, "invalid digit found in string"),
            IntErrorKind::PosOverflow => write!(f, "number too large to fit in {}", self.name),
            IntErrorKind::NegOverflow => write!(f, "number too small to fit in {}", self.name),
            IntErrorKind::Zero => {
                write!(f, "number would be zero for non-zero type ({})", self.name)
            }
            _ => write!(f, "Unexpected error parsing {}", self.name),
        }
    }
}

pub struct Radix(u32);
pub trait FromStrRadix: Sized + Copy {
    fn from_str_radix(x: &str, radix: u32) -> Result<Self, FromStrRadixError>;
}

impl<T: FromStrRadixInternal + Copy> FromStrRadix for T {
    fn from_str_radix(x: &str, radix: u32) -> Result<Self, FromStrRadixError> {
        <Self as FromStrRadixInternal>::from_str_radix(x, Radix(radix)).map_err(|error| {
            FromStrRadixError {
                name: <Self as FromStrRadixInternal>::NAME,
                error,
                radix,
            }
        })
    }
}

#[doc(hidden)]
pub trait FromStrRadixInternal: Sized {
    const NAME: &'static str;
    fn from_str_radix(x: &str, radix: Radix) -> Result<Self, ParseIntError>;
}

impl FromStrRadixInternal for u8 {
    fn from_str_radix(x: &str, radix: Radix) -> Result<Self, ParseIntError> {
        Self::from_str_radix(x, radix.0)
    }

    const NAME: &'static str = "byte";
}

impl FromStrRadixInternal for u16 {
    fn from_str_radix(x: &str, radix: Radix) -> Result<Self, ParseIntError> {
        Self::from_str_radix(x, radix.0)
    }

    const NAME: &'static str = "word";
}
