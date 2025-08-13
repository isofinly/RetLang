use std::collections::{HashMap, HashSet};

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
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum TempVar {
    PeekOff,
    PeekOffVal,
    SwapLeft,
    SwapLeftVal,
    SwapRight,
    SwapRightVal,
    SwapTemp,
    BinRes,
    AddrVal,
    CallRet,
    Off,
    OffVal,
    RetVal,
    MemOff,
    MemOffVal,
    UnaryRes,
}

impl<'a> CodeBuilder<'a> {
    pub fn new(ast: &'a Program) -> Self {
        CodeBuilder {
            ast,
            need_defines: false,
            need_typedefs: false,
            need_vars: false,
            need_includes: false,
            need_main: false,
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
                if self.ast.gadgets.iter().any(|g| &g.name == id) {
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

    fn analyze_used_variables(&self) -> std::collections::HashSet<TempVar> {
        let mut used = std::collections::HashSet::new();

        for gadget in &self.ast.gadgets {
            for instr in &gadget.body.instructions {
                self.collect_used_vars_from_instr(instr, &mut used);
            }
            if let Some(ret_expr) = &gadget.body.ret.value {
                self.collect_used_vars_from_expr(ret_expr, &mut used);
            }
        }

        used
    }

    fn collect_used_vars_from_instr(
        &self,
        instr: &Instruction,
        used: &mut std::collections::HashSet<TempVar>,
    ) {
        match instr {
            Instruction::StackOp(StackOp::Peek { .. }) => {
                used.insert(TempVar::PeekOff);
                used.insert(TempVar::PeekOffVal);
            }
            Instruction::StackOp(StackOp::Swap { .. }) => {
                used.insert(TempVar::SwapLeft);
                used.insert(TempVar::SwapLeftVal);
                used.insert(TempVar::SwapRight);
                used.insert(TempVar::SwapRightVal);
                used.insert(TempVar::SwapTemp);
            }
            Instruction::Arithmetic(_) => {
                used.insert(TempVar::BinRes);
            }
            Instruction::MemoryOp(_) => {
                used.insert(TempVar::AddrVal);
            }
            Instruction::ConditionalMod(cond_mod) => {
                self.collect_used_vars_from_expr(&cond_mod.condition.lhs, used);
                self.collect_used_vars_from_expr(&cond_mod.condition.rhs, used);
                self.collect_used_vars_from_expr(&cond_mod.value, used);
            }
            Instruction::Assignment(ass) => {
                self.collect_used_vars_from_expr(&ass.value, used);
            }

            Instruction::Call(_) => {
                used.insert(TempVar::CallRet);
            }
            _ => {}
        }
    }

    fn collect_used_vars_from_expr(
        &self,
        expr: &Expression,
        used: &mut std::collections::HashSet<TempVar>,
    ) {
        match expr {
            Expression::StackRef(_) => {
                used.insert(TempVar::Off);
                used.insert(TempVar::OffVal);
                used.insert(TempVar::RetVal);
            }
            Expression::MemoryRef(_) => {
                used.insert(TempVar::MemOff);
                used.insert(TempVar::MemOffVal);
            }
            Expression::Binary(bin) => {
                self.collect_used_vars_from_expr(&bin.lhs, used);
                self.collect_used_vars_from_expr(&bin.rhs, used);
            }
            Expression::Unary(_) => {
                used.insert(TempVar::UnaryRes);
            }
            Expression::Call(_) => {
                used.insert(TempVar::CallRet);
            }
            _ => {}
        }
    }

    pub fn build(self) -> String {
        let mut code = String::new();

        if self.need_defines {
            code.push_str(&format!("{}\n", super::constants::DEFINES));
        }

        if self.need_typedefs {
            code.push_str(&format!("{}\n", super::constants::TYPEDEFS));
        }

        if self.need_includes {
            let mut includes = String::from("#include <stdlib.h>\n");
            for header in &self.ast.headers {
                match header {
                    Header::System(parts) => {
                        includes.push_str(&format!("#include <{}>\n", parts.join(".")));
                    }
                    Header::User(s) => {
                        includes.push_str(&format!("#include \"{s}\"\n"));
                    }
                }
            }
            includes.push('\n');
            code.push_str(&includes);
        }

        if self.need_vars {
            let vars = self.populate_variables(self.ast);
            code.push_str(&format!("{}\n", vars));
        }

        if self.need_main {
            let mut main_code = String::from("int main() {\n");
            main_code.push_str("Value *stack = malloc(INITIAL_CAPACITY * sizeof(Value));\n");
            main_code.push_str("if (!stack) return ERR_ALLOC;\n");
            main_code.push_str("int sp = 0;\n");
            main_code.push_str("int capacity = INITIAL_CAPACITY;\n\n");
            main_code.push_str("// Variable declarations for C99 compliance\n");

            let used_vars = self.analyze_used_variables();
            if used_vars.contains(&TempVar::Off) {
                main_code.push_str("Value off;\nlong long off_val;\n");
            }
            if used_vars.contains(&TempVar::MemOff) {
                main_code.push_str("Value mem_off;\nlong long mem_off_val;\n");
            }
            if used_vars.contains(&TempVar::PeekOff) {
                main_code.push_str("Value peek_off;\nlong long peek_off_val;\n");
            }
            if used_vars.contains(&TempVar::SwapLeft) {
                main_code.push_str("Value swap_left;\nlong long swap_left_val;\nValue swap_right;\nlong long swap_right_val;\nValue swap_temp;\n");
            }
            if used_vars.contains(&TempVar::RetVal) {
                main_code.push_str("Value ret_val;\n");
            }
            main_code.push_str("Value next;\n");
            if used_vars.contains(&TempVar::BinRes) {
                main_code.push_str("Value bin_res;\n");
            }
            if used_vars.contains(&TempVar::AddrVal) {
                main_code.push_str("Value mem_addr;\nlong long addr_val;\n");
            }
            if used_vars.contains(&TempVar::UnaryRes) {
                main_code.push_str("Value unary_res;\n");
            }
            if used_vars.contains(&TempVar::CallRet) {
                main_code.push_str("Value call_ret;\n");
            }
            let num_gadgets = self.ast.gadgets.len();
            for i in 1..=num_gadgets {
                let var_name = if i == 1 {
                    "ret_top".to_string()
                } else {
                    format!("ret_top{i}")
                };
                main_code.push_str(&format!("Value {var_name};\n"));
            }
            main_code.push('\n');

            // Add memory array if memory operations are used
            if used_vars.contains(&TempVar::AddrVal) || used_vars.contains(&TempVar::MemOff) {
                main_code.push_str("// Memory array for load/store operations\nValue memory[MEMORY_SIZE];\n// Initialize memory to zero\nfor (int i = 0; i < MEMORY_SIZE; i++) {\nmemory[i] = (Value){.type = TYPE_INT, .u = {.int_val = 0LL}};\n}\n\n");
            }

            main_code.push_str("// Initialize stack from stack_init ([main] bottom to top)\n");
            for id in &self.ast.stack_init.initial {
                main_code.push_str(&format!("if (sp >= capacity) {{\ncapacity *= 2;\nstack = realloc(stack, capacity * sizeof(Value));\nif (!stack) return ERR_ALLOC;\n}}\nstack[sp++] = (Value){{.type = TYPE_LABEL, .u = {{.label_val = &&g_{id}_start}}}};\n"));
            }
            main_code.push_str("\nvoid *pc = &&start;\ngoto *pc;\n\nstart:\nif (sp <= 0) goto end;\nnext = stack[--sp];\nif (next.type != TYPE_LABEL) return ERR_TYPE;\npc = next.u.label_val;\ngoto *pc;\n\n");

            let mut ret_top_count = 1u32;
            let referenced_gadgets = self.find_referenced_gadgets();

            for gadget in &self.ast.gadgets {
                if gadget.name == "main" || referenced_gadgets.contains(&gadget.name) {
                    main_code.push_str(&self.generate_gadget_code(gadget, &mut ret_top_count));
                } else {
                    main_code.push_str(&format!("#pragma GCC diagnostic push\n#pragma GCC diagnostic ignored \"-Wunused-label\"\n{}#pragma GCC diagnostic pop\n", self.generate_gadget_code(gadget, &mut ret_top_count)));
                }
            }

            main_code.push_str("end_with_error_alloc:\nfree(stack);\nreturn ERR_ALLOC;\n\nend:\nif (sp > 0) {\nif (stack[sp - 1].type == TYPE_INT) {\nprintf(\"finish %lld\", stack[sp - 1].u.int_val);\n} else {\nfree(stack);\nreturn ERR_TYPE;\n}\n}\nfree(stack);\nreturn ERR_SUCCESS;\n}\n");
            code.push_str(&main_code);
        }

        code
    }

    fn generate_gadget_code(&self, gadget: &GadgetDef, ret_top_count: &mut u32) -> String {
        let mut code = String::new();
        let gadget_name = &gadget.name;
        let instructions = &gadget.body.instructions;

        if !instructions.is_empty() {
            let mut suffixes: Vec<String> = vec!["start".to_string()];
            let mut suffix_counts: HashMap<String, u32> = HashMap::new();

            for i in instructions.iter().skip(1) {
                let proposed = self.suffix_for_instr(i);
                let count = suffix_counts.entry(proposed.clone()).or_insert(0);
                *count += 1;
                let suff = if *count == 1 {
                    proposed
                } else {
                    format!("{proposed}_{count}")
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
                code.push_str(&format!("g_{gadget_name}_{label_suff}:\n// {}\n{}pc = &&g_{gadget_name}_{};\ngoto *pc;\n\n",
                    self.comment_for_instr(&instructions[i]),
                    self.generate_instr_code(&instructions[i]),
                    suffixes[i + 1]
                ));
            }

            let ret_label_suff = &suffixes[instructions.len()];
            code.push_str(&format!(
                "g_{gadget_name}_{ret_label_suff}:\n{}",
                self.generate_ret_code(&gadget.body.ret, ret_top_count)
            ));
        } else {
            code.push_str(&format!(
                "g_{gadget_name}_start:\n{}",
                self.generate_ret_code(&gadget.body.ret, ret_top_count)
            ));
        }

        *ret_top_count += 1;

        code
    }

    fn suffix_for_instr(&self, instr: &Instruction) -> String {
        match instr {
            Instruction::Assignment(_) => "assign".to_string(),
            Instruction::Arithmetic(_) => "arith".to_string(),
            Instruction::StackOp(StackOp::Push(expr)) => match expr {
                Expression::Literal(Literal::Int(i)) => format!("push{i}"),
                Expression::Identifier(id) if self.is_gadget(id) => format!("push_{id}"),
                _ => "push".to_string(),
            },
            Instruction::StackOp(StackOp::Pop(_)) => "pop".to_string(),
            Instruction::StackOp(StackOp::Peek { .. }) => "peek".to_string(),
            Instruction::StackOp(StackOp::Swap { .. }) => "swap".to_string(),
            Instruction::MemoryOp(MemoryOp::Store { .. }) => "store".to_string(),
            Instruction::MemoryOp(MemoryOp::Load { .. }) => "load".to_string(),
            Instruction::ConditionalMod(_) => "cond".to_string(),
            Instruction::Call(_) => "call".to_string(),
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
                c.push_str(&format!("if (sp >= capacity) {{\ncapacity *= 2;\nstack = realloc(stack, capacity * sizeof(Value));\nif (!stack) goto end_with_error_alloc;\n}}\nstack[sp++] = {val};\n"));
                c
            }
            Instruction::StackOp(StackOp::Pop(target)) => {
                format!("if (sp <= 0) return ERR_UNDERFLOW;\nv_{target} = stack[--sp];\n")
            }
            Instruction::StackOp(StackOp::Peek { target, offset }) => {
                let (mut c, val) = self.generate_expr(offset);
                c.push_str(&format!("peek_off = {val};\nif (peek_off.type != TYPE_INT) return ERR_TYPE;\npeek_off_val = peek_off.u.int_val;\nif (peek_off_val < 0 || peek_off_val >= sp) return ERR_BOUNDS;\nv_{target} = stack[sp - 1 - peek_off_val];\n"));
                c
            }
            Instruction::StackOp(StackOp::Swap { left, right }) => {
                let (mut c, left_val) = self.generate_expr(left);
                let (right_code, right_val) = self.generate_expr(right);
                c.push_str(&right_code);
                c.push_str(&format!("swap_left = {left_val};\nif (swap_left.type != TYPE_INT) return ERR_TYPE;\nswap_left_val = swap_left.u.int_val;\nif (swap_left_val < 0 || swap_left_val >= sp) return ERR_BOUNDS;\nswap_right = {right_val};\nif (swap_right.type != TYPE_INT) return ERR_TYPE;\nswap_right_val = swap_right.u.int_val;\nif (swap_right_val < 0 || swap_right_val >= sp) return ERR_BOUNDS;\nswap_temp = stack[sp - 1 - swap_left_val];\nstack[sp - 1 - swap_left_val] = stack[sp - 1 - swap_right_val];\nstack[sp - 1 - swap_right_val] = swap_temp;\n"));
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
                c.push_str(&format!("if ({v1}.type != TYPE_INT) return ERR_TYPE;\nif ({v2}.type != TYPE_INT) return ERR_TYPE;\nbin_res = (Value){{.type = TYPE_INT, .u = {{.int_val = {temp_val}}}}};\nif (v_{}.type != TYPE_INT) return ERR_TYPE;\nv_{}.u.int_val = v_{}.u.int_val {} bin_res.u.int_val;\n", arith.dest, arith.dest, arith.dest, op_str));
                c
            }
            Instruction::MemoryOp(mem_op) => match mem_op {
                MemoryOp::Store { value, address } => {
                    let (mut c, val_code) = self.generate_expr(value);
                    let (addr_code, addr_val) = self.generate_expr(address);
                    c.push_str(&addr_code);
                    c.push_str(&format!("mem_addr = {addr_val};\nif (mem_addr.type != TYPE_INT) return ERR_TYPE;\naddr_val = mem_addr.u.int_val;\nif (addr_val < 0 || addr_val >= MEMORY_SIZE) return ERR_MEMORY;\nmemory[addr_val] = {val_code};\n"));
                    c
                }
                MemoryOp::Load { target, address } => {
                    let (mut c, addr_val) = self.generate_expr(address);
                    c.push_str(&format!("mem_addr = {addr_val};\nif (mem_addr.type != TYPE_INT) return ERR_TYPE;\naddr_val = mem_addr.u.int_val;\nif (addr_val < 0 || addr_val >= MEMORY_SIZE) return ERR_MEMORY;\nv_{target} = memory[addr_val];\n"));
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
                c.push_str(&format!("if ({lhs_val}.type != TYPE_INT) return ERR_TYPE;\nif ({rhs_val}.type != TYPE_INT) return ERR_TYPE;\nif ({lhs_val}.u.int_val {comp_op} {rhs_val}.u.int_val) {{\nv_{} = {};\n}}\n", cond_mod.target, val_expr));
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

    fn generate_ret_code(&self, ret: &ReturnStmt, ret_top_count: &mut u32) -> String {
        let mut c = String::new();
        if let Some(expr) = &ret.value {
            let (setup_code, val) = self.generate_expr(expr);
            c.push_str(&setup_code);
            c.push_str(&format!("ret_val = {val};\n// Push the value\nif (sp >= capacity) {{\ncapacity *= 2;\nstack = realloc(stack, capacity * sizeof(Value));\nif (!stack) goto end_with_error_alloc;\n}}\nstack[sp++] = ret_val;\n"));
        }
        let ret_top_name = if *ret_top_count == 1 {
            "ret_top".to_string()
        } else {
            format!("ret_top{ret_top_count}")
        };
        c.push_str(&format!("// Then ret logic\nif (sp == 0) goto end;\n{ret_top_name} = stack[sp - 1];\nif ({ret_top_name}.type != TYPE_LABEL) goto end;\nsp--;  // pop\npc = {ret_top_name}.u.label_val;\ngoto *pc;\n"));
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
                    Literal::Str(s) => format!(
                        "(Value){{.type = TYPE_STR, .u = {{.str_val = \"{}\" }}}}",
                        s.replace("\"", "\\\"")
                    ),
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
                code.push_str(&format!("if ({val1}.type != TYPE_INT) return ERR_TYPE;\nif ({val2}.type != TYPE_INT) return ERR_TYPE;\n"));
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
                code.push_str(&format!("off = {val};\nif (off.type != TYPE_INT) return ERR_TYPE;\noff_val = off.u.int_val;\nif (off_val < 0 || off_val >= sp) return ERR_BOUNDS;\n"));
                let val = "stack[sp - 1 - off_val]".to_string();
                (code, val)
            }
            Expression::MemoryRef(e) => {
                let (code, val) = self.generate_expr(e);
                let mut code = code;
                code.push_str(&format!("mem_off = {val};\nif (mem_off.type != TYPE_INT) return ERR_TYPE;\nmem_off_val = mem_off.u.int_val;\nif (mem_off_val < 0 || mem_off_val >= MEMORY_SIZE) return ERR_MEMORY;\n"));
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
        self.ast.gadgets.iter().any(|g| g.name == id)
    }
}
