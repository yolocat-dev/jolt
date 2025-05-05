use crate::lexer::token::{Kind, Token};

#[derive(Debug)]
pub enum Node {
    File(Vec<Node>),
    Use(String),
    FunctionDef {
        public: bool,
        name: String,
        params: Vec<Node>,
        body: Option<Box<Node>>,
        return_type: Option<String>,
    },
    Parameter {
        name: String,
        type_: String,
    },
    Block(Vec<Node>),
    FunctionCall(String, Vec<Node>),
    Return(Box<Node>),
    BinaryExpr {
        left: Box<Node>,
        operator: Kind,
        right: Box<Node>,
    },
    UnaryExpr {
        operator: Kind,
        operand: Box<Node>,
    },
    Identifier(String),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    ISize(isize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    USize(usize),
    F32(f32),
    F64(f64),
    ImplicitInteger(String),
    ImplicitFloat(String),
    If {
        condition: Box<Node>,
        body: Box<Node>,
        else_ifs: Vec<(Box<Node>, Box<Node>)>,
        else_body: Option<Box<Node>>,
    },
    String(String),
    While {
        condition: Box<Node>,
        body: Box<Node>,
    },
    For {
        var_name: String,
        iterable: Box<Node>,
        body: Box<Node>,
    },
    VariableDecl {
        mutable: bool,
        name: String,
        type_: Option<String>,
        value: Option<Box<Node>>,
    },
    BlockReturn(Box<Node>),
    Struct {
        public: bool,
        attributes: Vec<String>,
        implements: Vec<String>,
        name: String,
        fields: Vec<Node>,
        methods: Vec<Node>,
    },
    StructField {
        name: String,
        type_: String,
        public: bool,
        mutable: bool,
        weak: bool,
        unowned: bool,
    },
    Trait {
        public: bool,
        name: String,
        implements: Vec<String>,
        methods: Vec<Node>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Assignment,
    Range,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    EqComparison,
    OrdComparison,
    BitwiseShift,
    Sum,
    Product,
    Unary,
}

impl Precedence {
    pub fn of(token: &Token) -> Precedence {
        match token.kind {
            Kind::Assign | Kind::PlusEq | Kind::MinusEq | Kind::MultiplyEq | Kind::DivideEq | Kind::ModuloEq | Kind::BitwiseAndEq | Kind::BitwiseOrEq | Kind::BitwiseXorEq | Kind::BitwiseShiftLeftEq | Kind::BitwiseShiftRightEq => Precedence::Assignment,
            Kind::RangeExclusive | Kind::RangeInclusive => Precedence::Range,
            Kind::LogicalOr => Precedence::LogicalOr,
            Kind::LogicalAnd => Precedence::LogicalAnd,
            Kind::BitwiseOr => Precedence::BitwiseOr,
            Kind::BitwiseXor => Precedence::BitwiseXor,
            Kind::BitwiseAnd => Precedence::BitwiseAnd,
            Kind::Eq | Kind::NotEq => Precedence::EqComparison,
            Kind::LessThan | Kind::LessThanEq | Kind::GreaterThan | Kind::GreaterThanEq => Precedence::OrdComparison,
            Kind::BitwiseShiftLeft | Kind::BitwiseShiftRight => Precedence::BitwiseShift,
            Kind::Plus | Kind::Minus => Precedence::Sum,
            Kind::Multiply | Kind::Divide | Kind::Modulo => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
}
