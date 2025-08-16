use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fmt::Write as _; // bring fmt::Write for write! on String

use crate::compiler::{
    ast::{
        ArithOp, BinOp, Call, CompOp, Expression, GadgetDef, Header, Identifier, Instruction,
        Literal, MemoryOp, Program, ReturnStmt, StackOp, UnaryOp,
    },
    semantics::track_local_variables,
};

pub struct CodeBuilder<'a> {
    ast: &'a Program<'a>,
    need_defines: bool,
    need_typedefs: bool,
    need_vars: bool,
    need_includes: bool,
    need_main: bool,
    gadget_names: HashSet<&'a str>,
}

bitflags::bitflags! {
    #[derive(Default)]
    struct TempVars: u32 {
        const PEEK_OFF        = 1 << 0;
        const PEEK_OFF_VAL    = 1 << 1;
        const SWAP_LEFT       = 1 << 2;
        const SWAP_LEFT_VAL   = 1 << 3;
        const SWAP_RIGHT      = 1 << 4;
        const SWAP_RIGHT_VAL  = 1 << 5;
        const SWAP_TEMP       = 1 << 6;
        const BIN_RES         = 1 << 7;
        const ADDR_VAL        = 1 << 8;
        const CALL_RET        = 1 << 9;
        const OFF             = 1 << 10;
        const OFF_VAL         = 1 << 11;
        const RET_VAL         = 1 << 12;
        const MEM_OFF         = 1 << 13;
        const MEM_OFF_VAL     = 1 << 14;
        const UNARY_RES       = 1 << 15;
    }
}

#[inline]
const fn grow_stack_snippet() -> &'static str {
    "if (sp >= capacity) {
capacity *= 2;
stack = realloc(stack, capacity * sizeof(Value));
if (!stack) goto end_with_error_alloc;
}
"
}

impl<'a> CodeBuilder<'a> {
    pub fn new(ast: &'a Program) -> Self {
        let gadget_names = ast.gadgets.iter().map(|g| g.name).collect::<HashSet<_>>();
        CodeBuilder {
            ast,
            need_defines: false,
            need_typedefs: false,
            need_vars: false,
            need_includes: false,
            need_main: false,
            gadget_names,
        }
    }

    #[allow(unused)]
    pub fn with_defines(mut self) -> Self {
        self.need_defines = true;
        self
    }

    #[allow(unused)]
    pub fn with_typedefs(mut self) -> Self {
        self.need_typedefs = true;
        self
    }

    #[allow(unused)]
    pub fn with_variables(mut self) -> Self {
        self.need_vars = true;
        self
    }

    #[allow(unused)]
    pub fn with_includes(mut self) -> Self {
        self.need_includes = true;
        self
    }

    #[allow(unused)]
    pub fn with_main(mut self) -> Self {
        self.need_main = true;
        self
    }

    pub fn with_all(mut self) -> Self {
        self.need_defines = true;
        self.need_typedefs = true;
        self.need_vars = true;
        self.need_includes = true;
        self.need_main = true;
        self
    }

    fn find_referenced_gadgets(&self) -> std::collections::HashSet<&'a str> {
        let mut referenced = std::collections::HashSet::new();

        for gadget in &self.ast.gadgets {
            for instr in &gadget.body.instructions {
                self.collect_gadget_refs_from_instr(instr, &mut referenced);
            }
            if let Some(ret_expr) = &gadget.body.ret.value {
                self.collect_gadget_refs_from_expr(ret_expr, &mut referenced);
            }
        }

        referenced
    }

    fn collect_gadget_refs_from_instr(
        &self,
        instr: &'a Instruction,
        refs: &mut std::collections::HashSet<&'a str>,
    ) {
        match instr {
            Instruction::StackOp(StackOp::Push(expr)) => {
                self.collect_gadget_refs_from_expr(expr, refs);
            }
            Instruction::StackOp(StackOp::Pop(_)) => {}
            Instruction::StackOp(StackOp::Peek { offset, .. }) => {
                self.collect_gadget_refs_from_expr(offset, refs);
            }
            Instruction::StackOp(StackOp::Swap { left, right }) => {
                self.collect_gadget_refs_from_expr(left, refs);
                self.collect_gadget_refs_from_expr(right, refs);
            }
            Instruction::Assignment(ass) => {
                self.collect_gadget_refs_from_expr(&ass.value, refs);
            }
            Instruction::Call(call) => {
                for arg in &call.args {
                    self.collect_gadget_refs_from_expr(arg, refs);
                }
            }
            Instruction::Arithmetic(arith) => {
                self.collect_gadget_refs_from_expr(&arith.lhs, refs);
                self.collect_gadget_refs_from_expr(&arith.rhs, refs);
            }
            Instruction::MemoryOp(mem_op) => match mem_op {
                MemoryOp::Store { value, address } => {
                    self.collect_gadget_refs_from_expr(value, refs);
                    self.collect_gadget_refs_from_expr(address, refs);
                }
                MemoryOp::Load { address, .. } => {
                    self.collect_gadget_refs_from_expr(address, refs);
                }
            },
            Instruction::ConditionalMod(cond_mod) => {
                self.collect_gadget_refs_from_expr(&cond_mod.condition.lhs, refs);
                self.collect_gadget_refs_from_expr(&cond_mod.condition.rhs, refs);
                self.collect_gadget_refs_from_expr(&cond_mod.value, refs);
            }
        }
    }

    fn collect_gadget_refs_from_expr(
        &self,
        expr: &'a Expression,
        refs: &mut std::collections::HashSet<&'a str>,
    ) {
        match expr {
            Expression::Identifier(id) => {
                if self.gadget_names.contains(id) {
                    refs.insert(id);
                }
            }
            Expression::Binary(bin) => {
                self.collect_gadget_refs_from_expr(&bin.lhs, refs);
                self.collect_gadget_refs_from_expr(&bin.rhs, refs);
            }
            Expression::Unary(un) => {
                self.collect_gadget_refs_from_expr(&un.operand, refs);
            }
            Expression::StackRef(inner) => {
                self.collect_gadget_refs_from_expr(inner, refs);
            }
            Expression::MemoryRef(inner) => {
                self.collect_gadget_refs_from_expr(inner, refs);
            }
            Expression::Call(call) => {
                for arg in &call.args {
                    self.collect_gadget_refs_from_expr(arg, refs);
                }
            }
            _ => {}
        }
    }

    fn analyze_used_variables(&self) -> TempVars {
        let mut used = TempVars::default();
        for gadget in &self.ast.gadgets {
            for instr in &gadget.body.instructions {
                self.collect_used_vars_from_instr_mask(instr, &mut used);
            }
            if let Some(ret_expr) = &gadget.body.ret.value {
                self.collect_used_vars_from_expr_mask(ret_expr, &mut used);
            }
        }
        used
    }

    fn collect_used_vars_from_instr_mask(&self, instr: &Instruction, used: &mut TempVars) {
        match instr {
            Instruction::StackOp(StackOp::Peek { .. }) => {
                used.insert(TempVars::PEEK_OFF | TempVars::PEEK_OFF_VAL);
            }
            Instruction::StackOp(StackOp::Swap { .. }) => {
                used.insert(
                    TempVars::SWAP_LEFT
                        | TempVars::SWAP_LEFT_VAL
                        | TempVars::SWAP_RIGHT
                        | TempVars::SWAP_RIGHT_VAL
                        | TempVars::SWAP_TEMP,
                );
            }
            Instruction::Arithmetic(_) => {
                used.insert(TempVars::BIN_RES);
            }
            Instruction::MemoryOp(_) => {
                used.insert(TempVars::ADDR_VAL);
            }
            Instruction::ConditionalMod(cond_mod) => {
                self.collect_used_vars_from_expr_mask(&cond_mod.condition.lhs, used);
                self.collect_used_vars_from_expr_mask(&cond_mod.condition.rhs, used);
                self.collect_used_vars_from_expr_mask(&cond_mod.value, used);
            }
            Instruction::Assignment(ass) => {
                self.collect_used_vars_from_expr_mask(&ass.value, used);
            }
            Instruction::Call(_) => {
                used.insert(TempVars::CALL_RET);
            }
            _ => {}
        }
    }

    fn collect_used_vars_from_expr_mask(&self, expr: &Expression, used: &mut TempVars) {
        match expr {
            Expression::StackRef(_) => {
                used.insert(TempVars::OFF | TempVars::OFF_VAL | TempVars::RET_VAL);
            }
            Expression::MemoryRef(_) => {
                used.insert(TempVars::MEM_OFF | TempVars::MEM_OFF_VAL);
            }
            Expression::Binary(bin) => {
                self.collect_used_vars_from_expr_mask(&bin.lhs, used);
                self.collect_used_vars_from_expr_mask(&bin.rhs, used);
            }
            Expression::Unary(_) => {
                used.insert(TempVars::UNARY_RES);
            }
            Expression::Call(_) => {
                used.insert(TempVars::CALL_RET);
            }
            _ => {}
        }
    }

    fn estimate_size(ast: &Program) -> usize {
        let insns: usize = ast.gadgets.iter().map(|g| g.body.instructions.len()).sum();
        256 + insns * 256 + ast.gadgets.len() * 256
    }

    pub fn build(self) -> String {
        let mut code = String::with_capacity(Self::estimate_size(self.ast));

        if self.need_defines {
            code.push_str(super::constants::DEFINES);
            code.push('\n');
        }

        if self.need_typedefs {
            code.push_str(super::constants::TYPEDEFS);
            code.push('\n');
        }

        if self.need_includes {
            let mut includes = String::with_capacity(self.ast.headers.len() * 32 + 64);
            includes.push_str("#include <stdlib.h>\n");
            for header in &self.ast.headers {
                match header {
                    Header::System(parts) => {
                        let mut path = String::new();
                        for (i, p) in parts.iter().enumerate() {
                            if i > 0 {
                                path.push('.');
                            }
                            path.push_str(p);
                        }
                        let _ = write!(includes, "#include <{}>\n", path);
                    }
                    Header::User(s) => {
                        let _ = write!(includes, "#include \"{}\"\n", s);
                    }
                }
            }
            includes.push('\n');
            code.push_str(&includes);
        }

        if self.need_vars {
            let vars = self.populate_variables(self.ast);
            code.push_str(&vars);
            code.push('\n');
        }

        if self.need_main {
            let mut main_code = String::from("int main() {\n");
            main_code.push_str(
                "Value *stack = malloc(INITIAL_CAPACITY * sizeof(Value));
                if (!stack) return ERR_ALLOC;
                int sp = 0;
                int capacity = INITIAL_CAPACITY;
                // Variable declarations for C99 compliance\n",
            );

            let used_vars = self.analyze_used_variables();
            if used_vars.contains(TempVars::OFF) {
                main_code.push_str(
                    "Value off;
                    long long off_val;",
                );
            }
            if used_vars.contains(TempVars::MEM_OFF) {
                main_code.push_str(
                    "Value mem_off;
                    long long mem_off_val;",
                );
            }
            if used_vars.contains(TempVars::PEEK_OFF) {
                main_code.push_str(
                    "Value peek_off;
                    long long peek_off_val;",
                );
            }
            if used_vars.contains(TempVars::SWAP_LEFT) {
                main_code.push_str(
                    "Value swap_left;
                    long long swap_left_val;
                    Value swap_right;
                    long long swap_right_val;
                    Value swap_temp;",
                );
            }
            if used_vars.contains(TempVars::RET_VAL) {
                main_code.push_str("Value ret_val;\n");
            }
            main_code.push_str("Value next;\n");
            if used_vars.contains(TempVars::BIN_RES) {
                main_code.push_str("Value bin_res;\n");
            }
            if used_vars.contains(TempVars::ADDR_VAL) {
                main_code.push_str(
                    "Value mem_addr;
                    long long addr_val;",
                );
            }
            if used_vars.contains(TempVars::UNARY_RES) {
                main_code.push_str("Value unary_res;\n");
            }
            if used_vars.contains(TempVars::CALL_RET) {
                main_code.push_str("Value call_ret;\n");
            }
            main_code.push_str("Value ret_top;\n");
            main_code.push('\n');

            // Add memory array if memory operations are used
            if used_vars.contains(TempVars::ADDR_VAL) || used_vars.contains(TempVars::MEM_OFF) {
                main_code.push_str(
                    "// Memory array for load/store operations
                    Value memory[MEMORY_SIZE];
                    // Initialize memory to zero
                    for (int i = 0; i < MEMORY_SIZE; i++) {
                    memory[i] = (Value){.type = TYPE_INT, .u = {.int_val = 0LL}};
                    }\n\n",
                );
            }

            main_code.push_str("// Initialize stack from stack_init ([main] bottom to top)\n");
            for id in &self.ast.stack_init.initial {
                let _ = write!(
                    main_code,
                    "{}stack[sp++] = (Value){{.type = TYPE_LABEL, .u = {{.label_val = &&g_{}_start}}}};\n",
                    grow_stack_snippet(),
                    id
                );
            }
            main_code.push_str(
                "void *pc = &&start;
                goto *pc;
                \n\n
                start:
                if (sp <= 0) goto end;
                next = stack[--sp];
                if (next.type != TYPE_LABEL) return ERR_TYPE;
                pc = next.u.label_val;
                goto *pc;
                \n\n",
            );

            let referenced_gadgets = self.find_referenced_gadgets();

            for gadget in &self.ast.gadgets {
                if gadget.name == "main" || referenced_gadgets.contains(&gadget.name) {
                    main_code.push_str(&self.generate_gadget_code(gadget));
                } else {
                    let body = self.generate_gadget_code(gadget);
                    main_code.push_str("#pragma GCC diagnostic push\n#pragma GCC diagnostic ignored \"-Wunused-label\"\n");
                    main_code.push_str(&body);
                    main_code.push_str("#pragma GCC diagnostic pop\n");
                }
            }

            main_code.push_str(
                "end_with_error_alloc:
                free(stack);
                return ERR_ALLOC;
                end:
                if (sp > 0) {
                if (stack[sp - 1].type == TYPE_INT) {
                printf(\"finish %lld\", stack[sp - 1].u.int_val);
                } else {
                free(stack);
                return ERR_TYPE;
                }
                }
                free(stack);
                return ERR_SUCCESS;
                }\n",
            );
            code.push_str(&main_code);
        }

        code
    }

    fn generate_gadget_code(&self, gadget: &GadgetDef) -> String {
        let mut code = String::new();
        let gadget_name = &gadget.name;
        let instructions = &gadget.body.instructions;

        if !instructions.is_empty() {
            let mut suffixes: Vec<String> = vec!["start".to_string()];
            let mut suffix_counts: HashMap<String, u32> = HashMap::new();

            for i in instructions.iter().skip(1) {
                let proposed = self.suffix_for_instr(i);
                let key = proposed.into_owned();
                let count = suffix_counts.entry(key.clone()).or_insert(0);
                *count += 1;
                let suff = if *count == 1 {
                    key
                } else {
                    format!("{key}_{count}")
                };
                suffixes.push(suff);
            }

            let proposed_ret = "ret".to_string();
            let ret_count = suffix_counts.entry(proposed_ret.clone()).or_insert(0);
            *ret_count += 1;
            let ret_suff = if *ret_count == 1 {
                proposed_ret
            } else {
                format!("{proposed_ret}_{ret_count}")
            };
            suffixes.push(ret_suff);

            for i in 0..instructions.len() {
                let label_suff = &suffixes[i];
                let _ = write!(
                    code,
                    "g_{}_{}:\n// {}\n{}pc = &&g_{}_{};\ngoto *pc;\n\n",
                    gadget_name,
                    label_suff,
                    self.comment_for_instr(&instructions[i]),
                    self.generate_instr_code(&instructions[i]),
                    gadget_name,
                    suffixes[i + 1]
                );
            }

            let ret_label_suff = &suffixes[instructions.len()];
            let _ = write!(
                code,
                "g_{}_{}:\n{}",
                gadget_name,
                ret_label_suff,
                self.generate_ret_code(&gadget.body.ret)
            );
        } else {
            let _ = write!(
                code,
                "g_{}_start:\n{}",
                gadget_name,
                self.generate_ret_code(&gadget.body.ret)
            );
        }

        code
    }

    fn suffix_for_instr(&self, instr: &Instruction) -> Cow<'static, str> {
        match instr {
            Instruction::Assignment(_) => Cow::Borrowed("assign"),
            Instruction::Arithmetic(_) => Cow::Borrowed("arith"),
            Instruction::StackOp(StackOp::Push(expr)) => match expr {
                Expression::Literal(Literal::Int(i)) => Cow::Owned(format!("push{i}")),
                Expression::Identifier(id) if self.is_gadget(id) => {
                    Cow::Owned(format!("push_{id}"))
                }
                _ => Cow::Borrowed("push"),
            },
            Instruction::StackOp(StackOp::Pop(_)) => Cow::Borrowed("pop"),
            Instruction::StackOp(StackOp::Peek { .. }) => Cow::Borrowed("peek"),
            Instruction::StackOp(StackOp::Swap { .. }) => Cow::Borrowed("swap"),
            Instruction::MemoryOp(MemoryOp::Store { .. }) => Cow::Borrowed("store"),
            Instruction::MemoryOp(MemoryOp::Load { .. }) => Cow::Borrowed("load"),
            Instruction::ConditionalMod(_) => Cow::Borrowed("cond"),
            Instruction::Call(_) => Cow::Borrowed("call"),
        }
    }

    fn comment_for_instr(&self, instr: &Instruction) -> String {
        match instr {
            Instruction::Assignment(ass) => format!("{} = {:?}", ass.target, ass.value),
            Instruction::StackOp(op) => match op {
                StackOp::Push(expr) => format!("push {expr:?}"),
                StackOp::Pop(id) => format!("pop {id}"),
                StackOp::Peek { target, offset } => format!("peek {target} [{offset:?}]"),
                StackOp::Swap { left, right } => format!("swap {left:?} {right:?}"),
            },
            Instruction::Arithmetic(arith) => format!(
                "{} {:?}= {:?} {:?} {:?}",
                arith.dest, arith.op, arith.lhs, arith.rhs_op, arith.rhs
            ),
            Instruction::MemoryOp(mem) => match mem {
                MemoryOp::Store { value, address } => format!("store {value:?} {address:?}"),
                MemoryOp::Load { target, address } => format!("load {target} {address:?}"),
            },
            Instruction::ConditionalMod(cond) => format!(
                "if {:?} {:?} {:?} then {} = {:?}",
                cond.condition.lhs, cond.condition.op, cond.condition.rhs, cond.target, cond.value
            ),
            Instruction::Call(call) => format!("call {} with args {:?}", call.callee, call.args),
        }
    }

    fn generate_instr_code(&self, instr: &Instruction) -> String {
        match instr {
            Instruction::StackOp(StackOp::Push(expr)) => {
                let (mut c, val) = self.generate_expr(expr);
                c.push_str(&format!(
                    "if (sp >= capacity) {{
                    capacity *= 2;
                    stack = realloc(stack, capacity * sizeof(Value));
                    if (!stack) goto end_with_error_alloc;
                    }}
                    stack[sp++] = {val};\n"
                ));
                c
            }
            Instruction::StackOp(StackOp::Pop(target)) => {
                format!("if (sp <= 0) return ERR_UNDERFLOW;\nv_{target} = stack[--sp];\n")
            }
            Instruction::StackOp(StackOp::Peek { target, offset }) => {
                let (mut c, val) = self.generate_expr(offset);
                c.push_str(&format!(
                    "peek_off = {val};
                    if (peek_off.type != TYPE_INT) return ERR_TYPE;
                    peek_off_val = peek_off.u.int_val;
                    if (peek_off_val < 0 || peek_off_val >= sp) return ERR_BOUNDS;
                    v_{target} = stack[sp - 1 - peek_off_val];\n"
                ));
                c
            }
            Instruction::StackOp(StackOp::Swap { left, right }) => {
                let (mut c, left_val) = self.generate_expr(left);
                let (right_code, right_val) = self.generate_expr(right);
                c.push_str(&right_code);
                c.push_str(&format!(
                    "swap_left = {left_val};
                    if (swap_left.type != TYPE_INT) return ERR_TYPE;
                    swap_left_val = swap_left.u.int_val;
                    if (swap_left_val < 0 || swap_left_val >= sp) return ERR_BOUNDS;
                    swap_right = {right_val};
                    if (swap_right.type != TYPE_INT) return ERR_TYPE;
                    swap_right_val = swap_right.u.int_val;
                    if (swap_right_val < 0 || swap_right_val >= sp) return ERR_BOUNDS;
                    swap_temp = stack[sp - 1 - swap_left_val];
                    stack[sp - 1 - swap_left_val] = stack[sp - 1 - swap_right_val];
                    stack[sp - 1 - swap_right_val] = swap_temp;\n"
                ));
                c
            }
            Instruction::Assignment(ass) => {
                let (mut c, val) = self.generate_expr(&ass.value);
                c.push_str(&format!("v_{} = {};\n", ass.target, val));
                c
            }
            Instruction::Call(call) => {
                let (c, _) = self.generate_call(call, false);
                c
            }
            Instruction::Arithmetic(arith) => {
                let (c1, v1) = self.generate_expr(&arith.lhs);
                let (c2, v2) = self.generate_expr(&arith.rhs);
                let mut c = c1 + &c2;
                let inner_op_str = self.get_op_str(&arith.rhs_op);
                let temp_val = format!("{v1}.u.int_val {inner_op_str} {v2}.u.int_val");
                let op_str = self.get_op_str(&arith.op);
                c.push_str(&format!(
                    "if ({v1}.type != TYPE_INT) return ERR_TYPE;
                    if ({v2}.type != TYPE_INT) return ERR_TYPE;
                    bin_res = (Value){{.type = TYPE_INT, .u = {{.int_val = {temp_val}}}}};
                    if (v_{}.type != TYPE_INT) return ERR_TYPE;
                    v_{}.u.int_val = v_{}.u.int_val {} bin_res.u.int_val;\n",
                    arith.dest, arith.dest, arith.dest, op_str
                ));
                c
            }
            Instruction::MemoryOp(mem_op) => match mem_op {
                MemoryOp::Store { value, address } => {
                    let (mut c, val_code) = self.generate_expr(value);
                    let (addr_code, addr_val) = self.generate_expr(address);
                    c.push_str(&addr_code);
                    c.push_str(&format!(
                        "mem_addr = {addr_val};
                        if (mem_addr.type != TYPE_INT) return ERR_TYPE;
                        addr_val = mem_addr.u.int_val;
                        if (addr_val < 0 || addr_val >= MEMORY_SIZE) return ERR_MEMORY;
                        memory[addr_val] = {val_code};\n"
                    ));
                    c
                }
                MemoryOp::Load { target, address } => {
                    let (mut c, addr_val) = self.generate_expr(address);
                    c.push_str(&format!(
                        "mem_addr = {addr_val};
                        if (mem_addr.type != TYPE_INT) return ERR_TYPE;
                        addr_val = mem_addr.u.int_val;
                        if (addr_val < 0 || addr_val >= MEMORY_SIZE) return ERR_MEMORY;
                        v_{target} = memory[addr_val];\n"
                    ));
                    c
                }
            },
            Instruction::ConditionalMod(cond_mod) => {
                let (mut c, lhs_val) = self.generate_expr(&cond_mod.condition.lhs);
                let (rhs_code, rhs_val) = self.generate_expr(&cond_mod.condition.rhs);
                c.push_str(&rhs_code);
                let (val_code, val_expr) = self.generate_expr(&cond_mod.value);
                c.push_str(&val_code);

                let comp_op = self.get_comp_op_str(&cond_mod.condition.op);
                c.push_str(&format!(
                    "if ({lhs_val}.type != TYPE_INT) return ERR_TYPE;
                    if ({rhs_val}.type != TYPE_INT) return ERR_TYPE;
                    if ({lhs_val}.u.int_val {comp_op} {rhs_val}.u.int_val) {{\nv_{} = {};\n}}\n",
                    cond_mod.target, val_expr
                ));
                c
            }
        }
    }

    fn get_op_str(&self, op: &ArithOp) -> &'static str {
        match op {
            ArithOp::Add => "+",
            ArithOp::Sub => "-",
            ArithOp::Mul => "*",
            ArithOp::Div => "/",
            ArithOp::Mod => "%",
            ArithOp::And => "&",
            ArithOp::Or => "|",
            ArithOp::Xor => "^",
            ArithOp::Shl => "<<",
            ArithOp::Shr => ">>",
        }
    }

    fn get_comp_op_str(&self, op: &CompOp) -> &'static str {
        match op {
            CompOp::Eq => "==",
            CompOp::Ne => "!=",
            CompOp::Lt => "<",
            CompOp::Le => "<=",
            CompOp::Gt => ">",
            CompOp::Ge => ">=",
        }
    }

    fn generate_ret_code(&self, ret: &ReturnStmt) -> String {
        let mut c = String::new();
        if let Some(expr) = &ret.value {
            let (setup_code, val) = self.generate_expr(expr);
            c.push_str(&setup_code);
            let _ = write!(
                c,
                "ret_val = {val};
                // Push the value
                {}stack[sp++] = ret_val;\n",
                grow_stack_snippet()
            );
        }
        c.push_str(
            "// Then ret logic
            if (sp == 0) goto end;
            ret_top = stack[sp - 1];
            if (ret_top.type != TYPE_LABEL) goto end;
            sp--;  // pop
            pc = ret_top.u.label_val;
            goto *pc;\n",
        );
        c
    }

    fn generate_call(&self, call: &Call, is_expr: bool) -> (String, Option<String>) {
        let mut c = String::new();
        let mut args_vals = Vec::new();
        for arg in &call.args {
            let (setup, val) = self.generate_expr(arg);
            c.push_str(&setup);
            args_vals.push(val);
        }
        c.push_str(
            "#pragma GCC diagnostic push\n#pragma GCC diagnostic ignored \"-Wint-conversion\"\n",
        );
        let mut unpacks = Vec::new();
        self.gen_call_branch(&mut c, 0, &mut unpacks, &args_vals, call.callee);
        c.push_str("#pragma GCC diagnostic pop\n");
        let ret_val = if is_expr {
            Some("call_ret".to_string())
        } else {
            None
        };
        (c, ret_val)
    }

    fn gen_call_branch(
        &self,
        c: &mut String,
        arg_index: usize,
        unpacks: &mut Vec<String>,
        args_vals: &Vec<String>,
        callee: &'a str,
    ) {
        if arg_index == args_vals.len() {
            c.push_str(&format!(
                "call_ret.type = TYPE_INT;\ncall_ret.u.int_val = {}({});\n",
                callee,
                unpacks.join(", ")
            ));
            return;
        }
        let val = &args_vals[arg_index];
        let types = [
            ("INT", "int_val"),
            ("STR", "str_val"),
            ("LABEL", "label_val"),
        ];
        for (i, (ty, field)) in types.iter().enumerate() {
            let prefix = if i == 0 { "" } else { "else " };
            c.push_str(&format!("{prefix}if ({val}.type == TYPE_{ty}) {{\n"));
            unpacks.push(format!("{val}.u.{field}"));
            self.gen_call_branch(c, arg_index + 1, unpacks, args_vals, callee);
            unpacks.pop();
            c.push_str("}\n");
        }
        c.push_str("else {\nreturn ERR_TYPE;\n}\n");
    }

    fn generate_expr(&self, expr: &Expression) -> (String, String) {
        match expr {
            Expression::Literal(lit) => (
                String::new(),
                match lit {
                    Literal::Int(i) => {
                        format!("(Value){{.type = TYPE_INT, .u = {{.int_val = {i}LL}}}}")
                    }
                    Literal::Str(s) => {
                        let mut lit = String::with_capacity(s.len() + 32);
                        lit.push_str("(Value){.type = TYPE_STR, .u = {.str_val = \"");
                        for b in s.bytes() {
                            lit.push(b as char)
                        }
                        lit.push_str("\" }}");
                        lit
                    }
                },
            ),
            Expression::Identifier(id) => (
                String::new(),
                if self.is_gadget(id) {
                    format!("(Value){{.type = TYPE_LABEL, .u = {{.label_val = &&g_{id}_start}}}}")
                } else {
                    format!("v_{id}")
                },
            ),
            Expression::Binary(b) => {
                let (code1, val1) = self.generate_expr(&b.lhs);
                let (code2, val2) = self.generate_expr(&b.rhs);
                let mut code = code1 + &code2;
                let op_str = match b.op {
                    BinOp::Add => "+",
                    BinOp::Sub => "-",
                    BinOp::Mul => "*",
                    BinOp::Div => "/",
                    BinOp::Mod => "%",
                    BinOp::And => "&",
                    BinOp::Or => "|",
                    BinOp::Xor => "^",
                    BinOp::Shl => "<<",
                    BinOp::Shr => ">>",
                };
                code.push_str(&format!(
                    "if ({val1}.type != TYPE_INT) return ERR_TYPE;
                    \nif ({val2}.type != TYPE_INT) return ERR_TYPE;\n"
                ));
                let val = format!(
                    "(Value){{.type = TYPE_INT, .u = {{.int_val = {val1}.u.int_val {op_str} {val2}.u.int_val}}}}"
                );
                (code, val)
            }
            Expression::Unary(u) => {
                let (code, val) = self.generate_expr(&u.operand);
                let mut c = code;
                let op_str = match u.op {
                    UnaryOp::Not => "!",
                };
                c.push_str(&format!("if ({val}.type != TYPE_INT) return ERR_TYPE;\n"));
                let res = format!(
                    "(Value){{.type = TYPE_INT, .u = {{.int_val = {op_str}{val}.u.int_val}}}}"
                );
                (c, res)
            }
            Expression::StackRef(e) => {
                let (code, val) = self.generate_expr(e);
                let mut code = code;
                code.push_str(&format!(
                    "off = {val};
                    if (off.type != TYPE_INT) return ERR_TYPE;
                    off_val = off.u.int_val;
                    if (off_val < 0 || off_val >= sp) return ERR_BOUNDS;\n"
                ));
                let val = "stack[sp - 1 - off_val]".to_string();
                (code, val)
            }
            Expression::MemoryRef(e) => {
                let (code, val) = self.generate_expr(e);
                let mut code = code;
                code.push_str(&format!(
                    "mem_off = {val};
                    if (mem_off.type != TYPE_INT) return ERR_TYPE;
                    mem_off_val = mem_off.u.int_val;
                    if (mem_off_val < 0 || mem_off_val >= MEMORY_SIZE) return ERR_MEMORY;\n"
                ));
                let val = "memory[mem_off_val]".to_string();
                (code, val)
            }
            Expression::Call(call) => {
                let (code, ret_val) = self.generate_call(call, true);
                (code, ret_val.unwrap())
            }
        }
    }

    fn populate_variables(&self, ast: &Program) -> String {
        let mut defs = String::new();
        let mut unique_identifiers: HashSet<Identifier> = HashSet::new();

        for gadget in &ast.gadgets {
            unique_identifiers.extend(track_local_variables(gadget).unwrap());
        }

        for identifier in unique_identifiers {
            defs.push_str(&format!("Value v_{identifier};\n"));
        }

        defs
    }

    fn is_gadget(&self, id: &str) -> bool {
        self.gadget_names.contains(id)
    }
}
