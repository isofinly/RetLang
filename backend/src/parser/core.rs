use crate::compiler::ast::*;
use crate::lexer::tokens::{Token, TokenKind};
use miette::{Result, miette};
use std::iter::Peekable;
use std::slice::Iter;

pub fn parse(tokens: &[Token]) -> Result<Program> {
    let mut p = Parser {
        iter: tokens.iter().peekable(),
    };
    p.program()
}

struct Parser<'t> {
    iter: Peekable<Iter<'t, Token>>,
}

impl<'t> Parser<'t> {
    fn peek(&mut self) -> Option<&'t TokenKind> {
        self.iter.peek().map(|t| &t.kind)
    }
    fn peek_second(&mut self) -> Option<&'t TokenKind> {
        let mut clone = self.iter.clone();
        clone.next()?;
        clone.next().map(|t| &t.kind)
    }
    fn next(&mut self) -> Option<&'t TokenKind> {
        self.iter.next().map(|t| &t.kind)
    }
    fn expect(&mut self, want: TokenKind) -> Result<()> {
        match self.next() {
            Some(k) if *k == want => Ok(()),
            _ => Err(miette!("expected `{want:?}` next token: {:?}", self.peek())),
        }
    }
    fn expect_identifier(&mut self) -> Result<String> {
        match self.next() {
            Some(TokenKind::Identifier(s)) => Ok(s.clone()),
            _ => Err(miette!("expected identifier")),
        }
    }

    fn is_stack_expression(&mut self) -> bool {
        let mut iter_clone = self.iter.clone();
        if let Some(first) = iter_clone.next() {
            if matches!(first.kind, TokenKind::Stack) {
                if let Some(second) = iter_clone.next() {
                    return matches!(second.kind, TokenKind::LBracket);
                }
            }
        }
        false
    }

    fn program(&mut self) -> Result<Program> {
        let mut gadgets = Vec::new();

        while matches!(self.peek(), Some(TokenKind::Gadget)) {
            gadgets.push(self.gadget_definition()?);
        }

        let stack_init = self.stack_init()?;
        self.expect(TokenKind::Eof)?;

        Ok(Program {
            gadgets,
            stack_init,
        })
    }

    fn gadget_definition(&mut self) -> Result<GadgetDef> {
        self.expect(TokenKind::Gadget)?;
        let name = self.expect_identifier()?;
        self.expect(TokenKind::Colon)?;
        let body = self.gadget_body()?;
        Ok(GadgetDef {
            name,
            body,
            doc: None,
        })
    }

    fn gadget_body(&mut self) -> Result<GadgetBody> {
        let mut instrs = Vec::new();
        while !matches!(self.peek(), Some(TokenKind::Ret)) {
            instrs.push(self.instruction()?);
        }
        let ret = self.return_stmt()?;
        Ok(GadgetBody {
            instructions: instrs,
            ret,
        })
    }

    fn instruction(&mut self) -> Result<Instruction> {
        Ok(match self.peek() {
            Some(TokenKind::Identifier(_)) => match self.peek_second() {
                Some(TokenKind::Assign) => Instruction::Assignment(self.assignment()?),
                Some(TokenKind::LParen) => Instruction::Call(self.call()?),
                _ => unreachable!(),
            },
            Some(TokenKind::Push | TokenKind::Pop | TokenKind::Peek | TokenKind::StackSwap) => {
                Instruction::StackOp(self.stack_op()?)
            }
            Some(TokenKind::Store | TokenKind::Load) => Instruction::MemoryOp(self.memory_op()?),
            Some(TokenKind::If) => Instruction::ConditionalMod(self.conditional_mod()?),
            _ => Instruction::Arithmetic(self.arithmetic()?),
        })
    }

    fn call(&mut self) -> Result<Call> {
        let callee = self.expect_identifier()?;
        self.expect(TokenKind::LParen)?;

        let mut args = Vec::new();
        if !matches!(self.peek(), Some(TokenKind::RParen)) {
            loop {
                args.push(self.expression()?);
                if matches!(self.peek(), Some(TokenKind::Comma)) {
                    self.next(); // consume comma
                } else {
                    break;
                }
            }
        }

        self.expect(TokenKind::RParen)?;
        Ok(Call { callee, args })
    }

    fn assignment(&mut self) -> Result<Assignment> {
        let target = self.expect_identifier()?;
        self.expect(TokenKind::Assign)?;
        let value = self.expression()?;
        Ok(Assignment { target, value })
    }

    fn stack_op(&mut self) -> Result<StackOp> {
        match self.next() {
            Some(TokenKind::Push) => {
                let v = self.expression()?;
                Ok(StackOp::Push(v))
            }
            Some(TokenKind::Pop) => {
                let id = self.expect_identifier()?;
                Ok(StackOp::Pop(id))
            }
            Some(TokenKind::Peek) => {
                let id = self.expect_identifier()?;
                self.expect(TokenKind::LBracket)?;
                let off = self.expression()?;
                self.expect(TokenKind::RBracket)?;
                Ok(StackOp::Peek {
                    target: id,
                    offset: off,
                })
            }
            Some(TokenKind::StackSwap) => {
                let a = self.expression()?;
                let b = self.expression()?;
                Ok(StackOp::Swap { left: a, right: b })
            }
            _ => Err(miette!("invalid stack op")),
        }
    }

    fn arithmetic(&mut self) -> Result<Arithmetic> {
        let dest = self.expect_identifier()?;
        let op1 = self.arith_op()?;
        self.expect(TokenKind::Assign)?;
        let lhs = self.expression()?;
        let op2 = self.arith_op()?;
        let rhs = self.expression()?;
        Ok(Arithmetic {
            dest,
            op: op1,
            lhs,
            rhs_op: op2,
            rhs,
        })
    }

    fn arith_op(&mut self) -> Result<ArithOp> {
        Ok(match self.next() {
            Some(TokenKind::Plus) => ArithOp::Add,
            Some(TokenKind::Minus) => ArithOp::Sub,
            Some(TokenKind::Star) => ArithOp::Mul,
            Some(TokenKind::Slash) => ArithOp::Div,
            Some(TokenKind::Percent) => ArithOp::Mod,
            Some(TokenKind::Amp) => ArithOp::And,
            Some(TokenKind::Pipe) => ArithOp::Or,
            Some(TokenKind::Caret) => ArithOp::Xor,
            Some(TokenKind::Shl) => ArithOp::Shl,
            Some(TokenKind::Shr) => ArithOp::Shr,
            _ => return Err(miette!("expected arithmetic operator")),
        })
    }

    fn memory_op(&mut self) -> Result<MemoryOp> {
        match self.next() {
            Some(TokenKind::Store) => {
                let val = self.expression()?;
                let addr = self.expression()?;
                Ok(MemoryOp::Store {
                    value: val,
                    address: addr,
                })
            }
            Some(TokenKind::Load) => {
                let target = self.expect_identifier()?;
                let addr = self.expression()?;
                Ok(MemoryOp::Load {
                    target,
                    address: addr,
                })
            }
            _ => Err(miette!("expected 'store' or 'load'")),
        }
    }

    fn conditional_mod(&mut self) -> Result<ConditionalMod> {
        self.expect(TokenKind::If)?;
        let cond = self.condition()?;
        self.expect(TokenKind::Then)?;
        let tgt = self.expect_identifier()?;
        self.expect(TokenKind::Assign)?;
        let val = self.expression()?;
        Ok(ConditionalMod {
            condition: cond,
            target: tgt,
            value: val,
        })
    }

    fn condition(&mut self) -> Result<Condition> {
        let lhs = self.expression()?;
        let op = self.comp_op()?;
        let rhs = self.expression()?;
        Ok(Condition { lhs, op, rhs })
    }

    fn comp_op(&mut self) -> Result<CompOp> {
        Ok(match self.next() {
            Some(TokenKind::EqEq) => CompOp::Eq,
            Some(TokenKind::NotEq) => CompOp::Ne,
            Some(TokenKind::Lt) => CompOp::Lt,
            Some(TokenKind::Le) => CompOp::Le,
            Some(TokenKind::Gt) => CompOp::Gt,
            Some(TokenKind::Ge) => CompOp::Ge,
            _ => return Err(miette!("expected comparison operator")),
        })
    }

    fn return_stmt(&mut self) -> Result<ReturnStmt> {
        self.expect(TokenKind::Ret)?;

        let val = if matches!(
            self.peek(),
            Some(
                TokenKind::Int(_) | TokenKind::Identifier(_) | TokenKind::LBracket | TokenKind::Not
            )
        ) || self.is_stack_expression()
        {
            Some(self.expression()?)
        } else {
            None
        };
        Ok(ReturnStmt { value: val })
    }

    fn stack_init(&mut self) -> Result<StackInit> {
        self.expect(TokenKind::Stack)?;
        self.expect(TokenKind::Colon)?;
        self.expect(TokenKind::LBracket)?;
        let mut ids = Vec::new();
        while !matches!(self.peek(), Some(TokenKind::RBracket)) {
            ids.push(self.expect_identifier()?);
            if matches!(self.peek(), Some(TokenKind::Comma)) {
                self.next();
            }
        }
        self.expect(TokenKind::RBracket)?;
        Ok(StackInit { initial: ids })
    }

    fn expression(&mut self) -> Result<Expression> {
        self.binary_expr(0)
    }

    fn binary_expr(&mut self, min_prec: usize) -> Result<Expression> {
        let mut left = self.primary()?;
        let precedence: &[&[TokenKind]] = &[
            &[TokenKind::Pipe],
            &[TokenKind::Caret],
            &[TokenKind::Amp],
            &[TokenKind::Plus, TokenKind::Minus],
            &[TokenKind::Star, TokenKind::Slash, TokenKind::Percent],
        ];

        for (prec, ops) in precedence.iter().enumerate().skip(min_prec) {
            while ops.iter().any(|k| matches!(self.peek(), Some(t) if t == k)) {
                let op_tok = self.next().unwrap().clone();
                let rhs = self.binary_expr(prec + 1)?;
                let op = match op_tok {
                    TokenKind::Plus => BinOp::Add,
                    TokenKind::Minus => BinOp::Sub,
                    TokenKind::Star => BinOp::Mul,
                    TokenKind::Slash => BinOp::Div,
                    TokenKind::Percent => BinOp::Mod,
                    TokenKind::Amp => BinOp::And,
                    TokenKind::Pipe => BinOp::Or,
                    TokenKind::Caret => BinOp::Xor,
                    _ => unreachable!(),
                };
                left = Expression::Binary(Box::new(BinaryExpr { lhs: left, op, rhs }));
            }
        }
        Ok(left)
    }

    fn primary(&mut self) -> Result<Expression> {
        Ok(match self.next() {
            Some(TokenKind::Int(i)) => Expression::Literal(Literal::Int(*i)),
            Some(TokenKind::Str(s)) => Expression::Literal(Literal::Str(s.clone())),
            Some(TokenKind::Identifier(id)) => {
                if matches!(self.peek_second(), Some(TokenKind::LParen)) {
                    return Ok(Expression::Call(Box::new(self.call()?)));
                } else {
                    Expression::Identifier(id.clone())
                }
            }
            Some(TokenKind::LBracket) => {
                let inner = self.expression()?;
                self.expect(TokenKind::RBracket)?;
                Expression::MemoryRef(Box::new(inner))
            }
            Some(TokenKind::Stack) => {
                self.expect(TokenKind::LBracket)?;
                let idx = self.expression()?;
                self.expect(TokenKind::RBracket)?;
                Expression::StackRef(Box::new(idx))
            }
            Some(TokenKind::Not) => {
                let operand = self.primary()?;
                Expression::Unary(Box::new(UnaryExpr {
                    op: UnaryOp::Not,
                    operand,
                }))
            }
            _ => return Err(miette!("unexpected token in expression")),
        })
    }
}
