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
        if let Some(k) = self.peek() {
            if *k == want {
                self.next();
                Ok(())
            } else {
                Err(miette!("expected `{want:?}`, got {k:?}"))
            }
        } else {
            Err(miette!("expected `{want:?}`, got None"))
        }
    }
    fn expect_identifier(&mut self) -> Result<String> {
        if let Some(TokenKind::Identifier(s)) = self.peek() {
            self.next();
            Ok(s.clone())
        } else {
            Err(miette!("expected identifier, got {:?}", self.peek()))
        }
    }
    fn expect_string(&mut self) -> Result<String> {
        if let Some(TokenKind::Str(s)) = self.peek() {
            self.next();
            Ok(s.clone())
        } else {
            Err(miette!("expected string literal, got {:?}", self.peek()))
        }
    }

    fn is_arith_op(&mut self, kind: &TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Amp
                | TokenKind::Pipe
                | TokenKind::Caret
                | TokenKind::Shl
                | TokenKind::Shr
        )
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
        let mut externals = Vec::new();
        while matches!(self.peek(), Some(TokenKind::External)) {
            externals.push(self.external()?);
        }

        let mut gadgets = Vec::new();

        while matches!(self.peek(), Some(TokenKind::Gadget)) {
            gadgets.push(self.gadget_definition()?);
        }

        let stack_init = self.stack_init()?;
        self.expect(TokenKind::Eof)?;

        Ok(Program {
            headers: externals,
            gadgets,
            stack_init,
        })
    }

    fn external(&mut self) -> Result<Header> {
        self.expect(TokenKind::External)?;
        self.expect(TokenKind::Colon)?;
        let header = match self.peek() {
            Some(TokenKind::Lt) => {
                self.expect(TokenKind::Lt)?;
                let mut parts: Vec<String> = vec![self.expect_identifier()?];
                while matches!(self.peek(), Some(TokenKind::Dot)) {
                    self.expect(TokenKind::Dot)?;
                    parts.push(self.expect_identifier()?);
                }
                self.expect(TokenKind::Gt)?;
                Header::System(parts)
            }
            Some(TokenKind::Str(_)) => {
                let path = self.expect_string()?;
                Header::User(path)
            }
            _ => return Err(miette!("expected '<' or '\"' after 'external:'")),
        };
        Ok(header)
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
                Some(k) if self.is_arith_op(k) => Instruction::Arithmetic(self.arithmetic()?),
                _ => {
                    return Err(miette!(
                        "Invalid instruction starting with identifier {:?}",
                        self.peek_second()
                    ));
                }
            },
            Some(TokenKind::Push | TokenKind::Pop | TokenKind::Peek | TokenKind::StackSwap) => {
                Instruction::StackOp(self.stack_op()?)
            }
            Some(TokenKind::Store | TokenKind::Load) => Instruction::MemoryOp(self.memory_op()?),
            Some(TokenKind::If) => Instruction::ConditionalMod(self.conditional_mod()?),
            _ => return Err(miette!("Invalid instruction")),
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
        // TODO: Change to expression??
        let lhs = self.primary()?;
        let op2 = self.arith_op()?;
        // TODO: Change to expression??
        let rhs = self.primary()?;
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
                TokenKind::Int(_)
                    | TokenKind::Str(_)
                    | TokenKind::Identifier(_)
                    | TokenKind::LBracket
                    | TokenKind::Not
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
            &[TokenKind::Shl, TokenKind::Shr],
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
                    TokenKind::Shl => BinOp::Shl,
                    TokenKind::Shr => BinOp::Shr,
                    _ => unreachable!(),
                };
                left = Expression::Binary(Box::new(BinaryExpr { lhs: left, op, rhs }));
            }
        }
        Ok(left)
    }

    fn primary(&mut self) -> Result<Expression> {
        match self.next() {
            Some(TokenKind::Int(i)) => Ok(Expression::Literal(Literal::Int(*i))),
            Some(TokenKind::Str(s)) => Ok(Expression::Literal(Literal::Str(s.clone()))),
            Some(TokenKind::Identifier(id)) => {
                if matches!(self.peek(), Some(TokenKind::LParen)) {
                    self.expect(TokenKind::LParen)?;
                    let mut args = Vec::new();
                    if !matches!(self.peek(), Some(TokenKind::RParen)) {
                        loop {
                            args.push(self.expression()?);
                            if matches!(self.peek(), Some(TokenKind::Comma)) {
                                self.next();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(TokenKind::RParen)?;
                    Ok(Expression::Call(Box::new(Call {
                        callee: id.clone(),
                        args,
                    })))
                } else {
                    Ok(Expression::Identifier(id.clone()))
                }
            }
            Some(TokenKind::LBracket) => {
                let inner = self.expression()?;
                self.expect(TokenKind::RBracket)?;
                Ok(Expression::MemoryRef(Box::new(inner)))
            }
            Some(TokenKind::Stack) => {
                self.expect(TokenKind::LBracket)?;
                let idx = self.expression()?;
                self.expect(TokenKind::RBracket)?;
                Ok(Expression::StackRef(Box::new(idx)))
            }
            Some(TokenKind::Not) => {
                let operand = self.primary()?;
                Ok(Expression::Unary(Box::new(UnaryExpr {
                    op: UnaryOp::Not,
                    operand,
                })))
            }
            _ => Err(miette!(
                "unexpected token in expression: {:?}",
                self.peek().unwrap()
            )),
        }
    }
}

mod tests {
    #![allow(unused)]
    use super::*;

    #[test]
    fn test_parse_external_header_1() -> miette::Result<()> {
        let tokens = vec![
            Token {
                kind: TokenKind::External,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 2,
                column: 9,
            },
            Token {
                kind: TokenKind::Lt,
                line: 2,
                column: 11,
            },
            Token {
                kind: TokenKind::Identifier("aboba".to_string()),
                line: 2,
                column: 12,
            },
            Token {
                kind: TokenKind::Dot,
                line: 2,
                column: 17,
            },
            Token {
                kind: TokenKind::Identifier("h".to_string()),
                line: 2,
                column: 18,
            },
            Token {
                kind: TokenKind::Gt,
                line: 2,
                column: 19,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 8,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 9,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 10,
            },
        ];
        let ast = parse(&tokens)?;

        let expected = Program {
            headers: vec![Header::System(vec!["aboba".to_string(), "h".to_string()])],
            gadgets: vec![],
            stack_init: StackInit { initial: vec![] },
        };

        assert_eq!(ast, expected);
        Ok(())
    }

    #[test]
    fn test_incorrect_external_header() -> miette::Result<()> {
        let tokens = vec![
            Token {
                kind: TokenKind::External,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 2,
                column: 9,
            },
            Token {
                kind: TokenKind::Identifier("aboba.h".to_string()),
                line: 2,
                column: 11,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 8,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 9,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 10,
            },
        ];
        let res = parse(&tokens);

        assert!(res.is_err());
        Ok(())
    }

    #[test]
    fn test_parse_multiple_external_headers() -> miette::Result<()> {
        let tokens = vec![
            Token {
                kind: TokenKind::External,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 2,
                column: 9,
            },
            Token {
                kind: TokenKind::Lt,
                line: 2,
                column: 11,
            },
            Token {
                kind: TokenKind::Identifier("aboba".to_string()),
                line: 2,
                column: 12,
            },
            Token {
                kind: TokenKind::Dot,
                line: 2,
                column: 17,
            },
            Token {
                kind: TokenKind::Identifier("h".to_string()),
                line: 2,
                column: 18,
            },
            Token {
                kind: TokenKind::Gt,
                line: 2,
                column: 19,
            },
            Token {
                kind: TokenKind::External,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 9,
            },
            Token {
                kind: TokenKind::Lt,
                line: 3,
                column: 11,
            },
            Token {
                kind: TokenKind::Identifier("aboba2".to_string()),
                line: 3,
                column: 12,
            },
            Token {
                kind: TokenKind::Dot,
                line: 3,
                column: 18,
            },
            Token {
                kind: TokenKind::Identifier("h".to_string()),
                line: 3,
                column: 19,
            },
            Token {
                kind: TokenKind::Gt,
                line: 3,
                column: 20,
            },
            Token {
                kind: TokenKind::Stack,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 4,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 4,
                column: 8,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 4,
                column: 9,
            },
            Token {
                kind: TokenKind::Eof,
                line: 4,
                column: 10,
            },
        ];
        let ast = parse(&tokens)?;

        let expected = Program {
            headers: vec![
                Header::System(vec!["aboba".to_string(), "h".to_string()]),
                Header::System(vec!["aboba2".to_string(), "h".to_string()]),
            ],
            gadgets: vec![],
            stack_init: StackInit { initial: vec![] },
        };

        assert_eq!(ast, expected);
        Ok(())
    }

    #[test]
    fn test_parse_external_with_gadget() -> miette::Result<()> {
        let tokens = vec![
            Token {
                kind: TokenKind::External,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 2,
                column: 9,
            },
            Token {
                kind: TokenKind::Lt,
                line: 2,
                column: 11,
            },
            Token {
                kind: TokenKind::Identifier("stdio".to_string()),
                line: 2,
                column: 12,
            },
            Token {
                kind: TokenKind::Dot,
                line: 2,
                column: 17,
            },
            Token {
                kind: TokenKind::Identifier("h".to_string()),
                line: 2,
                column: 18,
            },
            Token {
                kind: TokenKind::Gt,
                line: 2,
                column: 19,
            },
            Token {
                kind: TokenKind::Gadget,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("main".to_string()),
                line: 4,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 4,
                column: 12,
            },
            Token {
                kind: TokenKind::Ret,
                line: 5,
                column: 5,
            },
            Token {
                kind: TokenKind::Stack,
                line: 7,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 7,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 7,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("main".to_string()),
                line: 7,
                column: 9,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 7,
                column: 13,
            },
            Token {
                kind: TokenKind::Eof,
                line: 7,
                column: 14,
            },
        ];
        let ast = parse(&tokens)?;

        let expected = Program {
            headers: vec![Header::System(vec!["stdio".to_string(), "h".to_string()])],
            gadgets: vec![GadgetDef {
                doc: None,
                name: "main".to_string(),
                body: GadgetBody {
                    instructions: vec![],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit {
                initial: vec!["main".to_string()],
            },
        };

        assert_eq!(ast, expected);
        Ok(())
    }

    #[test]
    fn valid_program_minimal() {
        let input = [
            Token {
                kind: TokenKind::Stack,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 6,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 7,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 9,
            },
        ];
        let expected = Program {
            stack_init: StackInit { initial: vec![] },
            gadgets: vec![],
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn valid_program_multiple_gadgets() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 10,
            },
            Token {
                kind: TokenKind::Ret,
                line: 1,
                column: 11,
            },
            Token {
                kind: TokenKind::Gadget,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 2,
                column: 2,
            },
            Token {
                kind: TokenKind::Colon,
                line: 2,
                column: 9,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 11,
            },
            Token {
                kind: TokenKind::Stack,
                line: 2,
                column: 12,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 3,
                column: 10,
            },
            Token {
                kind: TokenKind::Comma,
                line: 3,
                column: 12,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 3,
                column: 14,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 16,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 17,
            },
        ];
        let expected = Program {
            gadgets: vec![
                GadgetDef {
                    doc: None,
                    name: "a".to_string(),
                    body: GadgetBody {
                        instructions: vec![],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "b".to_string(),
                    body: GadgetBody {
                        instructions: vec![],
                        ret: ReturnStmt { value: None },
                    },
                },
            ],
            stack_init: StackInit {
                initial: vec!["a".to_string(), "b".to_string()],
            },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn invalid_program_missing_stack_init() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 10,
            },
            Token {
                kind: TokenKind::Ret,
                line: 1,
                column: 11,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 14,
            },
        ];
        let program = parse(&input);

        assert!(program.is_err())
    }

    #[test]
    fn edge_program_empty_stack_with_gadgets() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("empty".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Ret,
                line: 1,
                column: 16,
            },
            Token {
                kind: TokenKind::Int(0),
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Stack,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 2,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 2,
                column: 8,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 2,
                column: 9,
            },
            Token {
                kind: TokenKind::Eof,
                line: 2,
                column: 10,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "empty".to_string(),
                body: GadgetBody {
                    instructions: vec![],
                    ret: ReturnStmt {
                        value: Some(Expression::Literal(Literal::Int(0))),
                    },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn valid_gadget_basic() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("test".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Push,
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 6,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 7,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "test".to_string(),
                body: GadgetBody {
                    instructions: vec![Instruction::StackOp(StackOp::Push(Expression::Literal(
                        Literal::Int(1),
                    )))],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn valid_gadget_multiple_instructions() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("multi".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 16,
            },
            Token {
                kind: TokenKind::Assign,
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 1,
                column: 19,
            },
            Token {
                kind: TokenKind::Push,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Int(2),
                line: 2,
                column: 2,
            },
            Token {
                kind: TokenKind::Pop,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 3,
                column: 6,
            },
            Token {
                kind: TokenKind::Ret,
                line: 4,
                column: 4,
            },
            Token {
                kind: TokenKind::Int(3),
                line: 4,
                column: 5,
            },
            Token {
                kind: TokenKind::Stack,
                line: 5,
                column: 3,
            },
            Token {
                kind: TokenKind::Colon,
                line: 5,
                column: 4,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 5,
                column: 10,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 5,
                column: 10,
            },
            Token {
                kind: TokenKind::Eof,
                line: 5,
                column: 10,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "multi".to_string(),
                body: GadgetBody {
                    instructions: vec![
                        Instruction::Assignment(Assignment {
                            target: "a".to_string(),
                            value: Expression::Literal(Literal::Int(1)),
                        }),
                        Instruction::StackOp(StackOp::Push(Expression::Literal(Literal::Int(2)))),
                        Instruction::StackOp(StackOp::Pop("b".to_string())),
                    ],
                    ret: ReturnStmt {
                        value: Some(Expression::Literal(Literal::Int(3))),
                    },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn invalid_gadget_missing_ret() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("no_ret".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::Push,
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 1,
                column: 22,
            },
            Token {
                kind: TokenKind::Stack,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 2,
                column: 8,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 2,
                column: 9,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 2,
                column: 10,
            },
            Token {
                kind: TokenKind::Eof,
                line: 2,
                column: 10,
            },
        ];
        let program = parse(&input);

        assert!(program.is_err())
    }

    #[test]
    fn edge_gadget_empty_body() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("empty".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Ret,
                line: 1,
                column: 16,
            },
            Token {
                kind: TokenKind::Stack,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 2,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 2,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 2,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 2,
                column: 5,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "empty".to_string(),
                body: GadgetBody {
                    instructions: vec![],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn valid_assignment_simple() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("assign".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenKind::Assign,
                line: 1,
                column: 19,
            },
            Token {
                kind: TokenKind::Int(42),
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "assign".to_string(),
                body: GadgetBody {
                    instructions: vec![Instruction::Assignment(Assignment {
                        target: "a".to_string(),
                        value: Expression::Literal(Literal::Int(42)),
                    })],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn invalid_assignment_missing_expr() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("bad".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 12,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Assign,
                line: 1,
                column: 16,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let program = parse(&input);

        assert!(program.is_err())
    }

    #[test]
    fn edge_assignment_nested_expr() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("nest".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::Assign,
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 1,
                column: 19,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 1,
                column: 21,
            },
            Token {
                kind: TokenKind::Plus,
                line: 1,
                column: 22,
            },
            Token {
                kind: TokenKind::Stack,
                line: 1,
                column: 24,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 30,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 1,
                column: 31,
            },
            Token {
                kind: TokenKind::Star,
                line: 1,
                column: 33,
            },
            Token {
                kind: TokenKind::Int(2),
                line: 1,
                column: 34,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 1,
                column: 36,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "nest".to_string(),
                body: GadgetBody {
                    instructions: vec![Instruction::Assignment(Assignment {
                        target: "a".to_string(),
                        value: Expression::Binary(Box::new(BinaryExpr {
                            lhs: Expression::MemoryRef(Box::new(Expression::Identifier(
                                "b".to_string(),
                            ))),
                            op: BinOp::Add,
                            rhs: Expression::StackRef(Box::new(Expression::Binary(Box::new(
                                BinaryExpr {
                                    lhs: Expression::Literal(Literal::Int(1)),
                                    op: BinOp::Mul,
                                    rhs: Expression::Literal(Literal::Int(2)),
                                },
                            )))),
                        })),
                    })],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn valid_stack_ops_all() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("stacks".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::Push,
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 1,
                column: 22,
            },
            Token {
                kind: TokenKind::Pop,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 2,
                column: 5,
            },
            Token {
                kind: TokenKind::Peek,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 3,
                column: 6,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 7,
            },
            Token {
                kind: TokenKind::Int(2),
                line: 3,
                column: 8,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 9,
            },
            Token {
                kind: TokenKind::StackSwap,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::Int(3),
                line: 4,
                column: 12,
            },
            Token {
                kind: TokenKind::Int(4),
                line: 4,
                column: 14,
            },
            Token {
                kind: TokenKind::Ret,
                line: 5,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 6,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 6,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 6,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 6,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 6,
                column: 5,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "stacks".to_string(),
                body: GadgetBody {
                    instructions: vec![
                        Instruction::StackOp(StackOp::Push(Expression::Literal(Literal::Int(1)))),
                        Instruction::StackOp(StackOp::Pop("a".to_string())),
                        Instruction::StackOp(StackOp::Peek {
                            target: "b".to_string(),
                            offset: Expression::Literal(Literal::Int(2)),
                        }),
                        Instruction::StackOp(StackOp::Swap {
                            left: Expression::Literal(Literal::Int(3)),
                            right: Expression::Literal(Literal::Int(4)),
                        }),
                    ],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn invalid_stack_op_wrong_args() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("bad".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 12,
            },
            Token {
                kind: TokenKind::Push,
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let program = parse(&input);

        assert!(program.is_err())
    }

    #[test]
    fn edge_stack_op_zero_offset() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("edge".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Peek,
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 21,
            },
            Token {
                kind: TokenKind::Int(0),
                line: 1,
                column: 22,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 1,
                column: 23,
            },
            Token {
                kind: TokenKind::StackSwap,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Int(0),
                line: 2,
                column: 11,
            },
            Token {
                kind: TokenKind::Str("zero".to_string()),
                line: 2,
                column: 13,
            },
            Token {
                kind: TokenKind::Ret,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 4,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 4,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 4,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 4,
                column: 5,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "edge".to_string(),
                body: GadgetBody {
                    instructions: vec![
                        Instruction::StackOp(StackOp::Peek {
                            target: "a".to_string(),
                            offset: Expression::Literal(Literal::Int(0)),
                        }),
                        Instruction::StackOp(StackOp::Swap {
                            left: Expression::Literal(Literal::Int(0)),
                            right: Expression::Literal(Literal::Str("zero".to_string())),
                        }),
                    ],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn valid_arithmetic_basic() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("arith".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 16,
            },
            Token {
                kind: TokenKind::Plus,
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::Assign,
                line: 1,
                column: 19,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 1,
                column: 21,
            },
            Token {
                kind: TokenKind::Minus,
                line: 1,
                column: 23,
            },
            Token {
                kind: TokenKind::Identifier("c".to_string()),
                line: 1,
                column: 24,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "arith".to_string(),
                body: GadgetBody {
                    instructions: vec![Instruction::Arithmetic(Arithmetic {
                        dest: "a".to_string(),
                        op: ArithOp::Add,
                        lhs: Expression::Identifier("b".to_string()),
                        rhs_op: ArithOp::Sub,
                        rhs: Expression::Identifier("c".to_string()),
                    })],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn invalid_arithmetic_missing_op() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("bad".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 12,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Plus,
                line: 1,
                column: 16,
            },
            Token {
                kind: TokenKind::Assign,
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 1,
                column: 19,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let program = parse(&input);

        assert!(program.is_err())
    }

    #[test]
    fn edge_arithmetic_multi_char_ops() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("edge".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::Shl,
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenKind::Assign,
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 1,
                column: 21,
            },
            Token {
                kind: TokenKind::Shr,
                line: 1,
                column: 23,
            },
            Token {
                kind: TokenKind::Identifier("c".to_string()),
                line: 1,
                column: 26,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "edge".to_string(),
                body: GadgetBody {
                    instructions: vec![Instruction::Arithmetic(Arithmetic {
                        dest: "a".to_string(),
                        op: ArithOp::Shl,
                        lhs: Expression::Identifier("b".to_string()),
                        rhs_op: ArithOp::Shr,
                        rhs: Expression::Identifier("c".to_string()),
                    })],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn valid_memory_ops_all() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("mem".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 12,
            },
            Token {
                kind: TokenKind::Store,
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 19,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 1,
                column: 21,
            },
            Token {
                kind: TokenKind::Load,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("c".to_string()),
                line: 2,
                column: 2,
            },
            Token {
                kind: TokenKind::Identifier("d".to_string()),
                line: 2,
                column: 7,
            },
            Token {
                kind: TokenKind::Ret,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 4,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 4,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 4,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 4,
                column: 5,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "mem".to_string(),
                body: GadgetBody {
                    instructions: vec![
                        Instruction::MemoryOp(MemoryOp::Store {
                            value: Expression::Identifier("a".to_string()),
                            address: Expression::Identifier("b".to_string()),
                        }),
                        Instruction::MemoryOp(MemoryOp::Load {
                            target: "c".to_string(),
                            address: Expression::Identifier("d".to_string()),
                        }),
                    ],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn invalid_memory_op_extra_args() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("bad".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 12,
            },
            Token {
                kind: TokenKind::Store,
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 19,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 1,
                column: 21,
            },
            Token {
                kind: TokenKind::Identifier("c".to_string()),
                line: 1,
                column: 23,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 2,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 6,
            },
        ];
        let program = parse(&input);

        assert!(program.is_err())
    }

    #[test]
    fn valid_conditional_basic() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("cond".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::If,
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenKind::EqEq,
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 1,
                column: 21,
            },
            Token {
                kind: TokenKind::Then,
                line: 1,
                column: 23,
            },
            Token {
                kind: TokenKind::Identifier("c".to_string()),
                line: 1,
                column: 28,
            },
            Token {
                kind: TokenKind::Assign,
                line: 1,
                column: 30,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 1,
                column: 32,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "cond".to_string(),
                body: GadgetBody {
                    instructions: vec![Instruction::ConditionalMod(ConditionalMod {
                        condition: Condition {
                            lhs: Expression::Identifier("a".to_string()),
                            op: CompOp::Eq,
                            rhs: Expression::Identifier("b".to_string()),
                        },
                        target: "c".to_string(),
                        value: Expression::Literal(Literal::Int(1)),
                    })],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn invalid_conditional_missing_then() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("bad".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 12,
            },
            Token {
                kind: TokenKind::If,
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 16,
            },
            Token {
                kind: TokenKind::Lt,
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 1,
                column: 19,
            },
            Token {
                kind: TokenKind::Identifier("c".to_string()),
                line: 1,
                column: 21,
            },
            Token {
                kind: TokenKind::Assign,
                line: 1,
                column: 23,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 1,
                column: 24,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let program = parse(&input);

        assert!(program.is_err())
    }

    #[test]
    fn edge_conditional_complex_condition() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("edge".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::If,
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 16,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 1,
                column: 19,
            },
            Token {
                kind: TokenKind::Plus,
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 1,
                column: 22,
            },
            Token {
                kind: TokenKind::Ge,
                line: 1,
                column: 24,
            },
            Token {
                kind: TokenKind::Stack,
                line: 1,
                column: 27,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 33,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 1,
                column: 34,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 1,
                column: 36,
            },
            Token {
                kind: TokenKind::Minus,
                line: 1,
                column: 37,
            },
            Token {
                kind: TokenKind::Int(2),
                line: 1,
                column: 39,
            },
            Token {
                kind: TokenKind::Then,
                line: 1,
                column: 41,
            },
            Token {
                kind: TokenKind::Identifier("c".to_string()),
                line: 1,
                column: 46,
            },
            Token {
                kind: TokenKind::Assign,
                line: 1,
                column: 48,
            },
            Token {
                kind: TokenKind::Str("str".to_string()),
                line: 1,
                column: 49,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "edge".to_string(),
                body: GadgetBody {
                    instructions: vec![Instruction::ConditionalMod(ConditionalMod {
                        condition: Condition {
                            lhs: Expression::Binary(Box::new(BinaryExpr {
                                lhs: Expression::MemoryRef(Box::new(Expression::Identifier(
                                    "a".to_string(),
                                ))),
                                op: BinOp::Add,
                                rhs: Expression::Literal(Literal::Int(1)),
                            })),
                            op: CompOp::Ge,
                            rhs: Expression::Binary(Box::new(BinaryExpr {
                                lhs: Expression::StackRef(Box::new(Expression::Identifier(
                                    "b".to_string(),
                                ))),
                                op: BinOp::Sub,
                                rhs: Expression::Literal(Literal::Int(2)),
                            })),
                        },
                        target: "c".to_string(),
                        value: Expression::Literal(Literal::Str("str".to_string())),
                    })],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn valid_expressions_all_types() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("expr".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Push,
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::Int(42),
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Push,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Str("str".to_string()),
                line: 2,
                column: 6,
            },
            Token {
                kind: TokenKind::Push,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 3,
                column: 6,
            },
            Token {
                kind: TokenKind::Push,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 4,
                column: 6,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 4,
                column: 7,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 4,
                column: 8,
            },
            Token {
                kind: TokenKind::Push,
                line: 5,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 5,
                column: 6,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 5,
                column: 12,
            },
            Token {
                kind: TokenKind::Identifier("c".to_string()),
                line: 5,
                column: 13,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 5,
                column: 14,
            },
            Token {
                kind: TokenKind::Push,
                line: 6,
                column: 1,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 6,
                column: 6,
            },
            Token {
                kind: TokenKind::Plus,
                line: 6,
                column: 8,
            },
            Token {
                kind: TokenKind::Int(2),
                line: 6,
                column: 9,
            },
            Token {
                kind: TokenKind::Star,
                line: 6,
                column: 11,
            },
            Token {
                kind: TokenKind::Int(3),
                line: 6,
                column: 12,
            },
            Token {
                kind: TokenKind::Ret,
                line: 7,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 8,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 8,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 8,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 8,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 8,
                column: 5,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "expr".to_string(),
                body: GadgetBody {
                    instructions: vec![
                        Instruction::StackOp(StackOp::Push(Expression::Literal(Literal::Int(42)))),
                        Instruction::StackOp(StackOp::Push(Expression::Literal(Literal::Str(
                            "str".to_string(),
                        )))),
                        Instruction::StackOp(StackOp::Push(Expression::Identifier(
                            "a".to_string(),
                        ))),
                        Instruction::StackOp(StackOp::Push(Expression::MemoryRef(Box::new(
                            Expression::Identifier("b".to_string()),
                        )))),
                        Instruction::StackOp(StackOp::Push(Expression::StackRef(Box::new(
                            Expression::Identifier("c".to_string()),
                        )))),
                        Instruction::StackOp(StackOp::Push(Expression::Binary(Box::new(
                            BinaryExpr {
                                lhs: Expression::Literal(Literal::Int(1)),
                                op: BinOp::Add,
                                rhs: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Literal(Literal::Int(2)),
                                    op: BinOp::Mul,
                                    rhs: Expression::Literal(Literal::Int(3)),
                                })),
                            },
                        )))),
                    ],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn invalid_expression_unbalanced() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("bad".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 12,
            },
            Token {
                kind: TokenKind::Push,
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 19,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let program = parse(&input);

        assert!(program.is_err())
    }

    #[test]
    fn edge_expression_deep_nesting() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("deep".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Push,
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 21,
            },
            Token {
                kind: TokenKind::Stack,
                line: 1,
                column: 22,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 28,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 29,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 30,
            },
            Token {
                kind: TokenKind::Plus,
                line: 1,
                column: 32,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 33,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 1,
                column: 34,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 1,
                column: 35,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 1,
                column: 36,
            },
            Token {
                kind: TokenKind::Star,
                line: 1,
                column: 38,
            },
            Token {
                kind: TokenKind::Identifier("c".to_string()),
                line: 1,
                column: 39,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 1,
                column: 40,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 1,
                column: 41,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 1,
                column: 42,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "deep".to_string(),
                body: GadgetBody {
                    instructions: vec![Instruction::StackOp(StackOp::Push(Expression::MemoryRef(
                        Box::new(Expression::MemoryRef(Box::new(Expression::StackRef(
                            Box::new(Expression::Binary(Box::new(BinaryExpr {
                                lhs: Expression::MemoryRef(Box::new(Expression::Binary(Box::new(
                                    BinaryExpr {
                                        lhs: Expression::Identifier("a".to_string()),
                                        op: BinOp::Add,
                                        rhs: Expression::MemoryRef(Box::new(
                                            Expression::Identifier("b".to_string()),
                                        )),
                                    },
                                )))),
                                op: BinOp::Mul,
                                rhs: Expression::Identifier("c".to_string()),
                            }))),
                        )))),
                    )))],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn invalid_expression_call() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("call".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Identifier("call".to_string()),
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::Identifier("foo".to_string()),
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let program = parse(&input);

        assert!(program.is_err())
    }

    #[test]
    fn valid_expression_unary() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("unary".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Push,
                line: 1,
                column: 16,
            },
            Token {
                kind: TokenKind::Not,
                line: 1,
                column: 21,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 22,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 2,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 6,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 7,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "unary".to_string(),
                body: GadgetBody {
                    instructions: vec![Instruction::StackOp(StackOp::Push(Expression::Unary(
                        Box::new(UnaryExpr {
                            op: UnaryOp::Not,
                            operand: Expression::Identifier("a".to_string()),
                        }),
                    )))],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn edge_expression_nested_unary() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("nested_unary".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 21,
            },
            Token {
                kind: TokenKind::Push,
                line: 1,
                column: 23,
            },
            Token {
                kind: TokenKind::Not,
                line: 1,
                column: 28,
            },
            Token {
                kind: TokenKind::Not,
                line: 1,
                column: 29,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 30,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 6,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 7,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "nested_unary".to_string(),
                body: GadgetBody {
                    instructions: vec![Instruction::StackOp(StackOp::Push(Expression::Unary(
                        Box::new(UnaryExpr {
                            op: UnaryOp::Not,
                            operand: Expression::Unary(Box::new(UnaryExpr {
                                op: UnaryOp::Not,
                                operand: Expression::Identifier("a".to_string()),
                            })),
                        }),
                    )))],
                    ret: ReturnStmt { value: None },
                },
            }],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn invalid_expression_postfix_not() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("bad_postfix".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Push,
                line: 1,
                column: 22,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 24,
            },
            Token {
                kind: TokenKind::Not,
                line: 1,
                column: 25,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 3,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 6,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 7,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 8,
            },
        ];
        let program = parse(&input);

        assert!(program.is_err())
    }

    #[test]
    fn valid_return_optional_expr() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("ret1".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Ret,
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::Gadget,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("ret2".to_string()),
                line: 2,
                column: 2,
            },
            Token {
                kind: TokenKind::Colon,
                line: 2,
                column: 9,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 11,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 2,
                column: 15,
            },
            Token {
                kind: TokenKind::Plus,
                line: 2,
                column: 16,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 2,
                column: 18,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let expected = Program {
            gadgets: vec![
                GadgetDef {
                    doc: None,
                    name: "ret1".to_string(),
                    body: GadgetBody {
                        instructions: vec![],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "ret2".to_string(),
                    body: GadgetBody {
                        instructions: vec![],
                        ret: ReturnStmt {
                            value: Some(Expression::Binary(Box::new(BinaryExpr {
                                lhs: Expression::Identifier("a".to_string()),
                                op: BinOp::Add,
                                rhs: Expression::Literal(Literal::Int(1)),
                            }))),
                        },
                    },
                },
            ],
            stack_init: StackInit { initial: vec![] },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }

    #[test]
    fn invalid_return_extra() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("bad".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 12,
            },
            Token {
                kind: TokenKind::Ret,
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Stack,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 2,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 2,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 2,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 2,
                column: 5,
            },
        ];
        let program = parse(&input);

        assert!(program.is_err())
    }

    #[test]
    fn invalid_overall_unknown_instruction() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("bad".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 12,
            },
            Token {
                kind: TokenKind::Identifier("unknown".to_string()),
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 1,
                column: 22,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 2,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 3,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 5,
            },
        ];
        let program = parse(&input);

        assert!(program.is_err())
    }

    #[test]
    fn edge_overall_full_program() {
        let input = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("full".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::Plus,
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenKind::Assign,
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::Identifier("b".to_string()),
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Minus,
                line: 1,
                column: 22,
            },
            Token {
                kind: TokenKind::Identifier("c".to_string()),
                line: 1,
                column: 23,
            },
            Token {
                kind: TokenKind::Store,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 2,
                column: 7,
            },
            Token {
                kind: TokenKind::Identifier("d".to_string()),
                line: 2,
                column: 8,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 2,
                column: 9,
            },
            Token {
                kind: TokenKind::Stack,
                line: 2,
                column: 11,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 2,
                column: 17,
            },
            Token {
                kind: TokenKind::Identifier("e".to_string()),
                line: 2,
                column: 18,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 2,
                column: 19,
            },
            Token {
                kind: TokenKind::If,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 3,
                column: 4,
            },
            Token {
                kind: TokenKind::Lt,
                line: 3,
                column: 6,
            },
            Token {
                kind: TokenKind::Int(2),
                line: 3,
                column: 7,
            },
            Token {
                kind: TokenKind::Then,
                line: 3,
                column: 9,
            },
            Token {
                kind: TokenKind::Identifier("f".to_string()),
                line: 3,
                column: 14,
            },
            Token {
                kind: TokenKind::Assign,
                line: 3,
                column: 16,
            },
            Token {
                kind: TokenKind::Str("g".to_string()),
                line: 3,
                column: 17,
            },
            Token {
                kind: TokenKind::Peek,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("h".to_string()),
                line: 4,
                column: 6,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 4,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("i".to_string()),
                line: 4,
                column: 9,
            },
            Token {
                kind: TokenKind::Percent,
                line: 4,
                column: 11,
            },
            Token {
                kind: TokenKind::Identifier("j".to_string()),
                line: 4,
                column: 12,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 4,
                column: 14,
            },
            Token {
                kind: TokenKind::StackSwap,
                line: 5,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("k".to_string()),
                line: 5,
                column: 11,
            },
            Token {
                kind: TokenKind::Identifier("l".to_string()),
                line: 5,
                column: 13,
            },
            Token {
                kind: TokenKind::Load,
                line: 6,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("m".to_string()),
                line: 6,
                column: 6,
            },
            Token {
                kind: TokenKind::Identifier("n".to_string()),
                line: 6,
                column: 8,
            },
            Token {
                kind: TokenKind::Pop,
                line: 7,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("o".to_string()),
                line: 7,
                column: 5,
            },
            Token {
                kind: TokenKind::Push,
                line: 8,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("p".to_string()),
                line: 8,
                column: 6,
            },
            Token {
                kind: TokenKind::Pipe,
                line: 8,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("q".to_string()),
                line: 8,
                column: 9,
            },
            Token {
                kind: TokenKind::Ret,
                line: 9,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("r".to_string()),
                line: 9,
                column: 5,
            },
            Token {
                kind: TokenKind::Caret,
                line: 9,
                column: 7,
            },
            Token {
                kind: TokenKind::Identifier("s".to_string()),
                line: 9,
                column: 8,
            },
            Token {
                kind: TokenKind::Stack,
                line: 10,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 10,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 10,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("t".to_string()),
                line: 10,
                column: 10,
            },
            Token {
                kind: TokenKind::Comma,
                line: 10,
                column: 11,
            },
            Token {
                kind: TokenKind::Identifier("u".to_string()),
                line: 10,
                column: 13,
            },
            Token {
                kind: TokenKind::Comma,
                line: 10,
                column: 14,
            },
            Token {
                kind: TokenKind::Identifier("v".to_string()),
                line: 10,
                column: 16,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 10,
                column: 17,
            },
            Token {
                kind: TokenKind::Eof,
                line: 10,
                column: 18,
            },
        ];
        let expected = Program {
            gadgets: vec![GadgetDef {
                doc: None,
                name: "full".to_string(),
                body: GadgetBody {
                    instructions: vec![
                        Instruction::Arithmetic(Arithmetic {
                            dest: "a".to_string(),
                            op: ArithOp::Add,
                            lhs: Expression::Identifier("b".to_string()),
                            rhs_op: ArithOp::Sub,
                            rhs: Expression::Identifier("c".to_string()),
                        }),
                        Instruction::MemoryOp(MemoryOp::Store {
                            value: Expression::MemoryRef(Box::new(Expression::Identifier(
                                "d".to_string(),
                            ))),
                            address: Expression::StackRef(Box::new(Expression::Identifier(
                                "e".to_string(),
                            ))),
                        }),
                        Instruction::ConditionalMod(ConditionalMod {
                            condition: Condition {
                                lhs: Expression::Literal(Literal::Int(1)),
                                op: CompOp::Lt,
                                rhs: Expression::Literal(Literal::Int(2)),
                            },
                            target: "f".to_string(),
                            value: Expression::Literal(Literal::Str("g".to_string())),
                        }),
                        Instruction::StackOp(StackOp::Peek {
                            target: "h".to_string(),
                            offset: Expression::Binary(Box::new(BinaryExpr {
                                lhs: Expression::Identifier("i".to_string()),
                                op: BinOp::Mod,
                                rhs: Expression::Identifier("j".to_string()),
                            })),
                        }),
                        Instruction::StackOp(StackOp::Swap {
                            left: Expression::Identifier("k".to_string()),
                            right: Expression::Identifier("l".to_string()),
                        }),
                        Instruction::MemoryOp(MemoryOp::Load {
                            target: "m".to_string(),
                            address: Expression::Identifier("n".to_string()),
                        }),
                        Instruction::StackOp(StackOp::Pop("o".to_string())),
                        Instruction::StackOp(StackOp::Push(Expression::Binary(Box::new(
                            BinaryExpr {
                                lhs: Expression::Identifier("p".to_string()),
                                op: BinOp::Or,
                                rhs: Expression::Identifier("q".to_string()),
                            },
                        )))),
                    ],
                    ret: ReturnStmt {
                        value: Some(Expression::Binary(Box::new(BinaryExpr {
                            lhs: Expression::Identifier("r".to_string()),
                            op: BinOp::Xor,
                            rhs: Expression::Identifier("s".to_string()),
                        }))),
                    },
                },
            }],
            stack_init: StackInit {
                initial: vec!["t".to_string(), "u".to_string(), "v".to_string()],
            },
            headers: vec![],
        };
        let program = parse(&input).unwrap();

        assert_eq!(program, expected)
    }
}
