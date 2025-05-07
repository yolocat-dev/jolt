#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: Kind,
    pub span: Span,
    pub value: Value,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kind {
    Eof,
    Comment,

    FunKeyword,
    ReturnKeyword,
    IfKeyword,
    ElseKeyword,
    ForKeyword,
    InKeyword,
    WhileKeyword,
    ValKeyword,
    MutKeyword,
    UseKeyword,
    StructKeyword,
    PubKeyword,
    WeakKeyword,
    UnownedKeyword,
    TraitKeyword,

    Identifier,
    StringLiteral,
    NumericLiteral,

    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,

    PlusEq,
    MinusEq,
    MultiplyEq,
    DivideEq,
    ModuloEq,

    GreaterThan,
    GreaterThanEq,
    LessThan,
    LessThanEq,
    Eq,
    NotEq,

    LogicalNot,
    LogicalAnd,
    LogicalOr,

    BitwiseNot,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseShiftLeft,
    BitwiseShiftRight,

    BitwiseAndEq,
    BitwiseOrEq,
    BitwiseXorEq,
    BitwiseShiftLeftEq,
    BitwiseShiftRightEq,

    Assign,
    Arrow,
    FatArrow,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    RangeExclusive,
    RangeInclusive,

    FieldAccess,
    Colon,
    Attribute,

    Comma,
    PathSeparator,
    Semicolon,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    None,
    String(String),
    Number(String),
}
