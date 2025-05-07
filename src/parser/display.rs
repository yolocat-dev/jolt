use std::fmt::Display;

use crate::lexer::token::Kind;

use super::node::Node;

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::File(segments) => {
                for u in segments.iter().filter(|n| matches!(*n, Node::Use(_))) {
                    writeln!(f, "{}", u)?;
                }

                for n in segments.iter().filter(|n| !matches!(*n, Node::Use(_))) {
                    writeln!(f, "\n{}", n)?;
                }
            },
            Node::Use(path) => {
                write!(f, "use {};", path)?;
            },
            Node::I8(num) => {
                write!(f, "{}i8", num)?;
            },
            Node::I16(num) => {
                write!(f, "{}i16", num)?;
            },
            Node::I32(num) => {
                write!(f, "{}i32", num)?;
            },
            Node::I64(num) => {
                write!(f, "{}i64", num)?;
            },
            Node::ISize(num) => {
                write!(f, "{}isize", num)?;
            },
            Node::U8(num) => {
                write!(f, "{}u8", num)?;
            },
            Node::U16(num) => {
                write!(f, "{}u16", num)?;
            },
            Node::U32(num) => {
                write!(f, "{}u32", num)?;
            },
            Node::U64(num) => {
                write!(f, "{}u64", num)?;
            },
            Node::USize(num) => {
                write!(f, "{}usize", num)?;
            },
            Node::F32(num) => {
                write!(f, "{}f32", num)?;
            },
            Node::F64(num) => {
                write!(f, "{}f64", num)?;
            },
            Node::ImplicitInteger(num) => {
                write!(f, "{}", num)?;
            },
            Node::ImplicitFloat(num) => {
                write!(f, "{}", num)?;
            },
            Node::If { condition, body, else_ifs, else_body } => {
                write!(f, "if {} {}", condition, body)?;

                for else_if in else_ifs {
                    write!(f, " else if {} ", else_if.0)?;

                    //let inner = format!("{}", else_if.1);
                    //write!(f, "{}", inner.replace('\n', "\n\t"))?;
                    write!(f, "{}", else_if.1)?;
                }

                if let Some(body) = else_body {
                    write!(f, " else ")?;

                    //let inner = format!("{}", body);
                    //write!(f, "{}", inner.replace('\n', "\n\t"))?;
                    write!(f, "{}", body)?;
                }
            },
            Node::For { var_name, iterable, body } => {
                write!(f, "for {} in {} ", var_name, iterable)?;

                //let inner = format!("{}", body);
                //write!(f, "{}", inner.replace('\n', "\n\t"))?;
                write!(f, "{}", body)?;
            },
            Node::Block(statements) => {
                writeln!(f, "{{")?;
                
                for stmt in statements {
                    let inner = format!("{}", stmt);

                    write!(f, "\t")?;

                    if matches!(stmt, Node::BlockReturn(_)) {
                        writeln!(f, "{}", inner.replace('\n', "\n\t"))?;
                    } else {
                        write!(f, "{}", inner.replace('\n', "\n\t"))?;
                        writeln!(f, ";")?;
                    }
                }

                write!(f, "}}")?;
            },
            Node::Return(expr) => {
                write!(f, "return {}", expr)?;
            },
            Node::BlockReturn(expr) => {
                write!(f, "{}", expr)?;
            },
            Node::String(value) => {
                write!(f, "\"{}\"", value)?;
            },
            Node::While { condition, body } => {
                write!(f, "while {} {}", condition, body)?;
            },
            Node::Trait { public, name, implements, methods } => {
                if *public {
                    write!(f, "pub ")?;
                }

                write!(f, "trait {}", name)?;

                if !implements.is_empty() {
                    write!(f, ": ")?;

                    for (i, t) in implements.iter().enumerate() {
                        write!(f, "{}", t)?;

                        if i < implements.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                }

                writeln!(f, " {{")?;

                for method in methods {
                    write!(f, "\t")?;

                    let inner = format!("{}", method);
                    writeln!(f, "{}", inner.replace('\n', "\n\t"))?;
                }

                writeln!(f, "}}")?;
            },
            Node::Struct { public, attributes, implements, name, fields, methods } => {
                if !attributes.is_empty() {
                    write!(f, "@[")?;
                    for (i, attr) in attributes.iter().enumerate() {
                        write!(f, "{}", attr)?;

                        if i < attributes.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    writeln!(f, "]")?;
                }

                if *public {
                    write!(f, "pub ")?;
                }

                write!(f, "struct {}", name)?;

                if !implements.is_empty() {
                    write!(f, ": ")?;

                    for (i, t) in implements.iter().enumerate() {
                        write!(f, "{}", t)?;

                        if i < implements.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                }

                writeln!(f, " {{")?;

                for field in fields {
                    write!(f, "\t")?;

                    let inner = format!("{}", field);
                    writeln!(f, "{}", inner.replace('\n', "\n\t"))?;
                }

                if !methods.is_empty() {
                    writeln!(f)?;

                    for method in methods {
                        write!(f, "\t")?;

                        let inner = format!("{}", method);
                        writeln!(f, "{}", inner.replace('\n', "\n\t"))?;
                    }
                }

                writeln!(f, "}}")?;
            },
            Node::Parameter { name, type_ } => {
                write!(f, "{}: {}", name, type_)?;
            },
            Node::UnaryExpr { operator, operand } => {
                write!(f, "{}", match operator {
                    Kind::Minus => "-",
                    Kind::LogicalNot => "!",
                    Kind::BitwiseNot => "~",
                    _ => unimplemented!("operator {:#?} is not a unary operator", operator),
                })?;

                write!(f, "{}", operand)?;
            },
            Node::Identifier(name) => {
                write!(f, "{}", name)?;
            },
            Node::BinaryExpr { left, operator, right } => {
                write!(f, "{} ", left)?;

                write!(f, "{}", match operator {
                    Kind::Assign => "=",
                    Kind::PlusEq => "+=",
                    Kind::MinusEq => "-=",
                    Kind::MultiplyEq => "*=",
                    Kind::DivideEq => "/=",
                    Kind::ModuloEq => "%=",
                    Kind::BitwiseAndEq => "&=",
                    Kind::BitwiseOrEq => "|=",
                    Kind::BitwiseXorEq => "^=",
                    Kind::BitwiseShiftLeftEq => "<<=",
                    Kind::BitwiseShiftRightEq => ">>=",
                    Kind::RangeExclusive => "..",
                    Kind::RangeInclusive => "..=",
                    Kind::LogicalOr => "||",
                    Kind::LogicalAnd => "&&",
                    Kind::BitwiseOr => "|",
                    Kind::BitwiseXor => "^",
                    Kind::BitwiseAnd => "&",
                    Kind::Eq => "==",
                    Kind::NotEq => "!=",
                    Kind::LessThan => "<",
                    Kind::LessThanEq => "<=",
                    Kind::GreaterThan => ">",
                    Kind::GreaterThanEq => ">=",
                    Kind::BitwiseShiftLeft => "<<=",
                    Kind::BitwiseShiftRight => ">>=",
                    Kind::Plus => "+",
                    Kind::Minus => "-",
                    Kind::Multiply => "*",
                    Kind::Divide => "/",
                    Kind::Modulo => "%",
                    _ => unimplemented!("token {:#?} is not an operator", operator),
                })?;

                write!(f, " {}", right)?;
            },
            Node::PathAccess { base, segment } => {
                write!(f, "{}::{}", base, segment)?;
            },
            Node::FunctionCall(func, args) => {
                write!(f, "{}(", func)?;

                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;

                    if i < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")?;
            },
            Node::FieldAccess { object, field } => {
                write!(f, "{}.{}", object, field)?;
            },
            Node::VariableDecl { mutable, name, type_, value } => {
                if *mutable {
                    write!(f, "mut ")?;
                }

                write!(f, "val {}", name)?;

                if let Some(t) = type_ {
                    write!(f, ": {}", t)?;
                }

                if let Some(v) = value {
                    write!(f, " = {}", v)?;
                }
            },
            Node::FunctionDef { public, name, params, body, return_type } => {
                if *public {
                    write!(f, "pub ")?;
                }

                write!(f, "fun {}(", name)?;

                for (i, param) in params.iter().enumerate() {
                    write!(f, "{}", param)?;

                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")?;

                if let Some(t) = return_type {
                    write!(f, " -> {}", t)?;
                }

                if let Some(b) = body {
                    write!(f, " ")?;

                    //let inner = format!("{}", b);
                    //writeln!(f, "{}", inner.replace('\n', "\n\t"))?;
                    write!(f, "{}", b)?;
                } else {
                    write!(f, ";")?;
                }
            },
            Node::StructField { name, type_, public, mutable, weak, unowned } => {
                if *public {
                    write!(f, "pub ")?;
                }

                if *unowned {
                    write!(f, "unowned ")?;
                } else if *weak {
                    write!(f, "weak ")?;
                }

                if *mutable {
                    write!(f, "mut ")?;
                }

                write!(f, "{}: {};", name, type_)?;
            }
        }

        Ok(())
    }
}
