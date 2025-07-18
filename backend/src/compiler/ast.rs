#![allow(dead_code)]
use std::boxed::Box;

pub type Identifier = String;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub gadgets: Vec<GadgetDef>,
    pub stack_init: StackInit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GadgetDef {
    pub doc: Option<String>,
    pub name: Identifier,
    pub body: GadgetBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GadgetBody {
    pub instructions: Vec<Instruction>,
    pub ret: ReturnStmt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Assignment(Assignment),
    StackOp(StackOp),
    Arithmetic(Arithmetic),
    MemoryOp(MemoryOp),
    ConditionalMod(ConditionalMod),
    Call(Call),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub target: Identifier,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StackOp {
    Push(Expression),
    Pop(Identifier),
    Peek {
        target: Identifier,
        offset: Expression,
    },
    Swap {
        left: Expression,
        right: Expression,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arithmetic {
    pub dest: Identifier, // the register / variable being updated
    pub op: ArithOp,      // `+`, `-`, …
    pub lhs: Expression,  // first rhs expression
    pub rhs_op: ArithOp,  // inner op between the two rhs exprs
    pub rhs: Expression,  // second rhs expression
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MemoryOp {
    Store {
        value: Expression,
        address: Expression,
    },
    Load {
        target: Identifier,
        address: Expression,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalMod {
    pub condition: Condition,
    pub target: Identifier,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Condition {
    pub lhs: Expression,
    pub op: CompOp,
    pub rhs: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<Expression>, // value to push before ‘ret’, if any
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    MemoryRef(Box<Expression>), // [expr]
    StackRef(Box<Expression>),  // stack[expr]
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Call(Box<Call>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub lhs: Expression,
    pub op: BinOp,
    pub rhs: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub operand: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StackInit {
    pub initial: Vec<Identifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Str(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Identifier,
    pub args: Vec<Expression>,
}
