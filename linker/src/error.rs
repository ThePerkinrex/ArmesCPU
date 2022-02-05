#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProgramLinkError {
    SymbolNotDefined(String, String),
    SymbolNotDeclared(String, String),
    /// Symbol, section, addr
    SegmentNotFound(String, u16),
    SegmentAddrNotFound(String, u16, u16),
    /// Symbol, og file, dup file
    DuplicateSymbol(String, String, String),
}
