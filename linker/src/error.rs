pub enum ProgramLinkError {
    SymbolNotDefined(String, String),
    SymbolNotDeclared(String, String),
}
