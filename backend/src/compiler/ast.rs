#![allow(dead_code)]
use std::boxed::Box;

pub type Identifier<'a> = &'a str;

#[derive(Debug, Clone, PartialEq)]
pub enum Header<'a> {
    System(Vec<Identifier<'a>>), // <part1.part2...>
    User(&'a str),               // "full/path.h"
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'a> {
    /// External C headers
    pub headers: Vec<Header<'a>>,
    pub gadgets: Vec<GadgetDef<'a>>,
    pub stack_init: StackInit<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GadgetDef<'a> {
    pub doc: Option<&'a str>,
    pub name: Identifier<'a>,
    pub body: GadgetBody<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GadgetBody<'a> {
    pub instructions: Vec<Instruction<'a>>,
    pub ret: ReturnStmt<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction<'a> {
    Assignment(Assignment<'a>),
    StackOp(StackOp<'a>),
    Arithmetic(Arithmetic<'a>),
    MemoryOp(MemoryOp<'a>),
    ConditionalMod(ConditionalMod<'a>),
    Call(Call<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment<'a> {
    pub target: Identifier<'a>,
    pub value: Expression<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StackOp<'a> {
    Push(Expression<'a>),
    Pop(Identifier<'a>),
    Peek {
        target: Identifier<'a>,
        offset: Expression<'a>,
    },
    Swap {
        left: Expression<'a>,
        right: Expression<'a>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arithmetic<'a> {
    pub dest: Identifier<'a>, // the register / variable being updated
    pub op: ArithOp,          // `+`, `-`, …
    pub lhs: Expression<'a>,  // first rhs expression
    pub rhs_op: ArithOp,      // inner op between the two rhs exprs
    pub rhs: Expression<'a>,  // second rhs expression
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
pub enum MemoryOp<'a> {
    Store {
        value: Expression<'a>,
        address: Expression<'a>,
    },
    Load {
        target: Identifier<'a>,
        address: Expression<'a>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalMod<'a> {
    pub condition: Condition<'a>,
    pub target: Identifier<'a>,
    pub value: Expression<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Condition<'a> {
    pub lhs: Expression<'a>,
    pub op: CompOp,
    pub rhs: Expression<'a>,
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
pub struct ReturnStmt<'a> {
    pub value: Option<Expression<'a>>, // value to push before ‘ret’, if any
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    Literal(Literal<'a>),
    Identifier(Identifier<'a>),
    MemoryRef(Box<Expression<'a>>), // [expr]
    StackRef(Box<Expression<'a>>),  // stack[expr]
    Binary(Box<BinaryExpr<'a>>),
    Unary(Box<UnaryExpr<'a>>),
    Call(Box<Call<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr<'a> {
    pub lhs: Expression<'a>,
    pub op: BinOp,
    pub rhs: Expression<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr<'a> {
    pub op: UnaryOp,
    pub operand: Expression<'a>,
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
pub struct StackInit<'a> {
    pub initial: Vec<Identifier<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'a> {
    Int(i64),
    Str(&'a str),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call<'a> {
    pub callee: Identifier<'a>,
    pub args: Vec<Expression<'a>>,
}
