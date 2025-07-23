use std::collections::{HashMap, HashSet};

use crate::compiler::{
    ast::{
        ArithOp, BinOp, Call, CompOp, Expression, GadgetDef, Header, Identifier, Instruction,
        Literal, MemoryOp, Program, ReturnStmt, StackOp, UnaryOp,
    },
    semantics::track_local_variables,
};

pub struct CodeBuilder<'a> {
    ast: &'a Program,
    need_defines: bool,
    need_typedefs: bool,
    need_vars: bool,
    need_includes: bool,
    need_main: bool,
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

    fn find_referenced_gadgets(&self) -> std::collections::HashSet<String> {
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
        instr: &Instruction,
        refs: &mut std::collections::HashSet<String>,
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
        expr: &Expression,
        refs: &mut std::collections::HashSet<String>,
    ) {
        match expr {
            Expression::Identifier(id) => {
                if self.ast.gadgets.iter().any(|g| &g.name == id) {
                    refs.insert(id.clone());
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

    fn analyze_used_variables(&self) -> std::collections::HashSet<&'static str> {
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
        used: &mut std::collections::HashSet<&'static str>,
    ) {
        match instr {
            Instruction::StackOp(StackOp::Peek { .. }) => {
                used.insert("peek_off");
                used.insert("peek_off_val");
            }
            Instruction::StackOp(StackOp::Swap { .. }) => {
                used.insert("swap_left");
                used.insert("swap_left_val");
                used.insert("swap_right");
                used.insert("swap_right_val");
                used.insert("swap_temp");
            }
            Instruction::Arithmetic(_) => {
                used.insert("bin_res");
            }
            Instruction::MemoryOp(_) => {
                used.insert("addr_val");
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
                used.insert("call_ret");
            }
            _ => {}
        }
    }

    fn collect_used_vars_from_expr(
        &self,
        expr: &Expression,
        used: &mut std::collections::HashSet<&'static str>,
    ) {
        match expr {
            Expression::StackRef(_) => {
                used.insert("off");
                used.insert("off_val");
                used.insert("ret_val");
            }
            Expression::MemoryRef(_) => {
                used.insert("mem_off");
                used.insert("mem_off_val");
            }
            Expression::Binary(bin) => {
                self.collect_used_vars_from_expr(&bin.lhs, used);
                self.collect_used_vars_from_expr(&bin.rhs, used);
            }
            Expression::Unary(_) => {
                used.insert("unary_res");
            }
            Expression::Call(_) => {
                used.insert("call_ret");
            }
            _ => {}
        }
    }

    pub fn build(self) -> String {
        let mut code = String::new();

        if self.need_defines {
            code.push_str(super::constants::DEFINES);
            code.push('\n');
        }

        if self.need_typedefs {
            code.push_str(super::constants::TYPEDEFS);
            code.push('\n');
        }

        if self.need_includes {
            code.push_str("#include <stdlib.h>\n");
            for header in &self.ast.headers {
                match header {
                    Header::System(parts) => {
                        code.push_str(&format!("#include <{}>\n", parts.join(".")));
                    }
                    Header::User(s) => {
                        code.push_str(&format!("#include \"{s}\"\n"));
                    }
                }
            }
            code.push('\n');
        }

        if self.need_vars {
            let vars = self.populate_variables(self.ast);
            code.push_str(&vars);
            code.push('\n');
        }

        if self.need_main {
            code.push_str("int main() {\n");
            code.push_str("Value *stack = malloc(INITIAL_CAPACITY * sizeof(Value));\n");
            code.push_str("if (!stack) return ERR_ALLOC;\n");
            code.push_str("int sp = 0;\n");
            code.push_str("int capacity = INITIAL_CAPACITY;\n\n");
            code.push_str("// Variable declarations for C99 compliance\n");
            let used_vars = self.analyze_used_variables();
            if used_vars.contains("off") {
                code.push_str("Value off;\n");
                code.push_str("long long off_val;\n");
            }
            if used_vars.contains("mem_off") {
                code.push_str("Value mem_off;\n");
                code.push_str("long long mem_off_val;\n");
            }
            if used_vars.contains("peek_off") {
                code.push_str("Value peek_off;\n");
                code.push_str("long long peek_off_val;\n");
            }
            if used_vars.contains("swap_left") {
                code.push_str("Value swap_left;\n");
                code.push_str("long long swap_left_val;\n");
                code.push_str("Value swap_right;\n");
                code.push_str("long long swap_right_val;\n");
                code.push_str("Value swap_temp;\n");
            }
            if used_vars.contains("ret_val") {
                code.push_str("Value ret_val;\n");
            }
            code.push_str("Value next;\n");
            if used_vars.contains("bin_res") {
                code.push_str("Value bin_res;\n");
            }
            if used_vars.contains("addr_val") {
                code.push_str("Value mem_addr;\n");
                code.push_str("long long addr_val;\n");
            }
            if used_vars.contains("unary_res") {
                code.push_str("Value unary_res;\n");
            }
            if used_vars.contains("call_ret") {
                code.push_str("Value call_ret;\n");
            }
            let num_gadgets = self.ast.gadgets.len();
            for i in 1..=num_gadgets {
                let var_name = if i == 1 {
                    "ret_top".to_string()
                } else {
                    format!("ret_top{}", i)
                };
                code.push_str(&format!("Value {};\n", var_name));
            }
            code.push('\n');

            // Add memory array if memory operations are used
            if used_vars.contains("addr_val") || used_vars.contains("mem_off") {
                code.push_str("// Memory array for load/store operations\n");
                code.push_str("Value memory[MEMORY_SIZE];\n");
                code.push_str("// Initialize memory to zero\n");
                code.push_str("for (int i = 0; i < MEMORY_SIZE; i++) {\n");
                code.push_str("memory[i] = (Value){.type = TYPE_INT, .u = {.int_val = 0LL}};\n");
                code.push_str("}\n\n");
            }

            code.push_str("// Initialize stack from stack_init ([main] bottom to top)\n");
            for id in &self.ast.stack_init.initial {
                code.push_str("if (sp >= capacity) {\n");
                code.push_str("capacity *= 2;\n");
                code.push_str("stack = realloc(stack, capacity * sizeof(Value));\n");
                code.push_str("if (!stack) return ERR_ALLOC;\n");
                code.push_str("}\n");
                code.push_str(&format!("stack[sp++] = (Value){{.type = TYPE_LABEL, .u = {{.label_val = &&g_{}_start}}}};\n", id));
            }
            code.push('\n');
            code.push_str("void *pc = &&start;\n");
            code.push_str("goto *pc;\n\n");
            code.push_str("start:\n");
            code.push_str("if (sp <= 0) goto end;\n");
            code.push_str("next = stack[--sp];\n");
            code.push_str("if (next.type != TYPE_LABEL) return ERR_TYPE;\n");
            code.push_str("pc = next.u.label_val;\n");
            code.push_str("goto *pc;\n\n");

            let mut ret_top_count = 1u32;
            let referenced_gadgets = self.find_referenced_gadgets();

            for gadget in &self.ast.gadgets {
                if gadget.name == "main" || referenced_gadgets.contains(&gadget.name) {
                    code.push_str(&self.generate_gadget_code(gadget, &mut ret_top_count));
                } else {
                    code.push_str("#pragma GCC diagnostic push\n");
                    code.push_str("#pragma GCC diagnostic ignored \"-Wunused-label\"\n");
                    code.push_str(&self.generate_gadget_code(gadget, &mut ret_top_count));
                    code.push_str("#pragma GCC diagnostic pop\n");
                }
            }

            code.push_str("end_with_error_alloc:\n");
            code.push_str("free(stack);\n");
            code.push_str("return ERR_ALLOC;\n\n");
            code.push_str("end:\n");
            code.push_str("if (sp > 0) {\n");
            code.push_str("if (stack[sp - 1].type == TYPE_INT) {\n");
            code.push_str("printf(\"finish %lld\", stack[sp - 1].u.int_val);\n");
            code.push_str("} else {\n");
            code.push_str("free(stack);\n");
            code.push_str("return ERR_TYPE;\n");
            code.push_str("}\n");
            code.push_str("}\n");
            code.push_str("free(stack);\n");
            code.push_str("return ERR_SUCCESS;\n");
            code.push_str("}\n");
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
                    format!("{}_{}", proposed, *count)
                };
                suffixes.push(suff);
            }

            let proposed_ret = "ret".to_string();
            let ret_count = suffix_counts.entry(proposed_ret.clone()).or_insert(0);
            *ret_count += 1;
            let ret_suff = if *ret_count == 1 {
                proposed_ret
            } else {
                format!("{}_{}", proposed_ret, *ret_count)
            };
            suffixes.push(ret_suff);

            for i in 0..instructions.len() {
                let label_suff = &suffixes[i];
                code.push_str(&format!("g_{}_{}:\n", gadget_name, label_suff));
                code.push_str(&format!(
                    "// {}\n",
                    self.comment_for_instr(&instructions[i])
                ));
                code.push_str(&self.generate_instr_code(&instructions[i]));
                code.push_str(&format!("pc = &&g_{}_{};\n", gadget_name, suffixes[i + 1]));
                code.push_str("goto *pc;\n\n");
            }

            let ret_label_suff = &suffixes[instructions.len()];
            code.push_str(&format!("g_{}_{}:\n", gadget_name, ret_label_suff));
            code.push_str(&self.generate_ret_code(&gadget.body.ret, ret_top_count));
        } else {
            code.push_str(&format!("g_{}_start:\n", gadget_name));
            code.push_str(&self.generate_ret_code(&gadget.body.ret, ret_top_count));
        }

        *ret_top_count += 1;

        code
    }

    fn suffix_for_instr(&self, instr: &Instruction) -> String {
        match instr {
            Instruction::Assignment(_) => "assign".to_string(),
            Instruction::Arithmetic(_) => "arith".to_string(),
            Instruction::StackOp(StackOp::Push(expr)) => match expr {
                Expression::Literal(Literal::Int(i)) => format!("push{}", i),
                Expression::Identifier(id) if self.is_gadget(id) => format!("push_{}", id),
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
                StackOp::Push(expr) => format!("push {:?}", expr),
                StackOp::Pop(id) => format!("pop {}", id),
                StackOp::Peek { target, offset } => format!("peek {} [{:?}]", target, offset),
                StackOp::Swap { left, right } => format!("swap {:?} {:?}", left, right),
            },
            Instruction::Arithmetic(arith) => format!(
                "{} {:?}= {:?} {:?} {:?}",
                arith.dest, arith.op, arith.lhs, arith.rhs_op, arith.rhs
            ),
            Instruction::MemoryOp(mem) => match mem {
                MemoryOp::Store { value, address } => format!("store {:?} {:?}", value, address),
                MemoryOp::Load { target, address } => format!("load {} {:?}", target, address),
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
                c.push_str("if (sp >= capacity) {\n");
                c.push_str("capacity *= 2;\n");
                c.push_str("stack = realloc(stack, capacity * sizeof(Value));\n");
                c.push_str("if (!stack) goto end_with_error_alloc;\n");
                c.push_str("}\n");
                c.push_str(&format!("stack[sp++] = {};\n", val));
                c
            }
            Instruction::StackOp(StackOp::Pop(target)) => format!(
                "if (sp <= 0) return ERR_UNDERFLOW;\nv_{} = stack[--sp];\n",
                target
            ),
            Instruction::StackOp(StackOp::Peek { target, offset }) => {
                let (mut c, val) = self.generate_expr(offset);
                c.push_str(&format!("peek_off = {};\n", val));
                c.push_str("if (peek_off.type != TYPE_INT) return ERR_TYPE;\n");
                c.push_str("peek_off_val = peek_off.u.int_val;\n");
                c.push_str("if (peek_off_val < 0 || peek_off_val >= sp) return ERR_BOUNDS;\n");
                c.push_str(&format!("v_{} = stack[sp - 1 - peek_off_val];\n", target));
                c
            }
            Instruction::StackOp(StackOp::Swap { left, right }) => {
                let (mut c, left_val) = self.generate_expr(left);
                let (right_code, right_val) = self.generate_expr(right);
                c.push_str(&right_code);
                c.push_str(&format!("swap_left = {};\n", left_val));
                c.push_str("if (swap_left.type != TYPE_INT) return ERR_TYPE;\n");
                c.push_str("swap_left_val = swap_left.u.int_val;\n");
                c.push_str("if (swap_left_val < 0 || swap_left_val >= sp) return ERR_BOUNDS;\n");
                c.push_str(&format!("swap_right = {};\n", right_val));
                c.push_str("if (swap_right.type != TYPE_INT) return ERR_TYPE;\n");
                c.push_str("swap_right_val = swap_right.u.int_val;\n");
                c.push_str("if (swap_right_val < 0 || swap_right_val >= sp) return ERR_BOUNDS;\n");
                c.push_str("swap_temp = stack[sp - 1 - swap_left_val];\n");
                c.push_str("stack[sp - 1 - swap_left_val] = stack[sp - 1 - swap_right_val];\n");
                c.push_str("stack[sp - 1 - swap_right_val] = swap_temp;\n");
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
                c.push_str(&format!("if ({}.type != TYPE_INT) return ERR_TYPE;\n", v1));
                c.push_str(&format!("if ({}.type != TYPE_INT) return ERR_TYPE;\n", v2));
                let inner_op_str = self.get_op_str(&arith.rhs_op);
                let temp_val = format!("{}.u.int_val {} {}.u.int_val", v1, inner_op_str, v2);
                c.push_str(&format!(
                    "bin_res = (Value){{.type = TYPE_INT, .u = {{.int_val = {}}}}};\n",
                    temp_val
                ));
                c.push_str(&format!(
                    "if (v_{}.type != TYPE_INT) return ERR_TYPE;\n",
                    arith.dest
                ));
                let op_str = self.get_op_str(&arith.op);
                c.push_str(&format!(
                    "v_{}.u.int_val = v_{}.u.int_val {} bin_res.u.int_val;\n",
                    arith.dest, arith.dest, op_str
                ));
                c
            }
            Instruction::MemoryOp(mem_op) => match mem_op {
                MemoryOp::Store { value, address } => {
                    let (mut c, val_code) = self.generate_expr(value);
                    let (addr_code, addr_val) = self.generate_expr(address);
                    c.push_str(&addr_code);
                    c.push_str(&format!("mem_addr = {};\n", addr_val));
                    c.push_str("if (mem_addr.type != TYPE_INT) return ERR_TYPE;\n");
                    c.push_str("addr_val = mem_addr.u.int_val;\n");
                    c.push_str("if (addr_val < 0 || addr_val >= MEMORY_SIZE) return ERR_MEMORY;\n");
                    c.push_str(&format!("memory[addr_val] = {};\n", val_code));
                    c
                }
                MemoryOp::Load { target, address } => {
                    let (mut c, addr_val) = self.generate_expr(address);
                    c.push_str(&format!("mem_addr = {};\n", addr_val));
                    c.push_str("if (mem_addr.type != TYPE_INT) return ERR_TYPE;\n");
                    c.push_str("addr_val = mem_addr.u.int_val;\n");
                    c.push_str("if (addr_val < 0 || addr_val >= MEMORY_SIZE) return ERR_MEMORY;\n");
                    c.push_str(&format!("v_{} = memory[addr_val];\n", target));
                    c
                }
            },
            Instruction::ConditionalMod(cond_mod) => {
                let (mut c, lhs_val) = self.generate_expr(&cond_mod.condition.lhs);
                let (rhs_code, rhs_val) = self.generate_expr(&cond_mod.condition.rhs);
                c.push_str(&rhs_code);
                let (val_code, val_expr) = self.generate_expr(&cond_mod.value);
                c.push_str(&val_code);

                c.push_str(&format!(
                    "if ({}.type != TYPE_INT) return ERR_TYPE;\n",
                    lhs_val
                ));
                c.push_str(&format!(
                    "if ({}.type != TYPE_INT) return ERR_TYPE;\n",
                    rhs_val
                ));

                let comp_op = self.get_comp_op_str(&cond_mod.condition.op);
                c.push_str(&format!(
                    "if ({}.u.int_val {} {}.u.int_val) {{\n",
                    lhs_val, comp_op, rhs_val
                ));
                c.push_str(&format!("v_{} = {};\n", cond_mod.target, val_expr));
                c.push_str("}\n");
                c
            }
        }
    }

    fn get_op_str(&self, op: &ArithOp) -> String {
        match op {
            ArithOp::Add => "+".to_string(),
            ArithOp::Sub => "-".to_string(),
            ArithOp::Mul => "*".to_string(),
            ArithOp::Div => "/".to_string(),
            ArithOp::Mod => "%".to_string(),
            ArithOp::And => "&".to_string(),
            ArithOp::Or => "|".to_string(),
            ArithOp::Xor => "^".to_string(),
            ArithOp::Shl => "<<".to_string(),
            ArithOp::Shr => ">>".to_string(),
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
            c.push_str(&format!("ret_val = {};\n", val));
            c.push_str("// Push the value\n");
            c.push_str("if (sp >= capacity) {\n");
            c.push_str("capacity *= 2;\n");
            c.push_str("stack = realloc(stack, capacity * sizeof(Value));\n");
            c.push_str("if (!stack) goto end_with_error_alloc;\n");
            c.push_str("}\n");
            c.push_str("stack[sp++] = ret_val;\n");
        }
        c.push_str("// Then ret logic\n");
        c.push_str("if (sp == 0) goto end;\n");
        let ret_top_name = if *ret_top_count == 1 {
            "ret_top".to_string()
        } else {
            format!("ret_top{}", ret_top_count)
        };
        c.push_str(&format!("{} = stack[sp - 1];\n", ret_top_name));
        c.push_str(&format!(
            "if ({}.type != TYPE_LABEL) goto end;\n",
            ret_top_name
        ));
        c.push_str("sp--;  // pop\n");
        c.push_str(&format!("pc = {}.u.label_val;\n", ret_top_name));
        c.push_str("goto *pc;\n");
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
        c.push_str("#pragma GCC diagnostic push\n");
        c.push_str("#pragma GCC diagnostic ignored \"-Wint-conversion\"\n");
        let mut unpacks: Vec<String> = Vec::new();
        self.gen_call_branch(&mut c, 0, &mut unpacks, &args_vals, &call.callee);
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
        callee: &String,
    ) {
        if arg_index == args_vals.len() {
            c.push_str("call_ret.type = TYPE_INT;\n");
            c.push_str(&format!(
                "call_ret.u.int_val = {}({});\n",
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
            c.push_str(&format!("{}if ({}.type == TYPE_{}) {{\n", prefix, val, ty));
            unpacks.push(format!("{}.u.{}", val, field));
            self.gen_call_branch(c, arg_index + 1, unpacks, args_vals, callee);
            unpacks.pop();
            c.push_str("}\n");
        }
        c.push_str("else {\n");
        c.push_str("return ERR_TYPE;\n");
        c.push_str("}\n");
    }

    fn generate_expr(&self, expr: &Expression) -> (String, String) {
        match expr {
            Expression::Literal(lit) => (
                String::new(),
                match lit {
                    Literal::Int(i) => {
                        format!("(Value){{.type = TYPE_INT, .u = {{.int_val = {}LL}}}}", i)
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
                    format!(
                        "(Value){{.type = TYPE_LABEL, .u = {{.label_val = &&g_{}_start}}}}",
                        id
                    )
                } else {
                    format!("v_{}", id)
                },
            ),
            Expression::Binary(b) => {
                let (code1, val1) = self.generate_expr(&b.lhs);
                let (code2, val2) = self.generate_expr(&b.rhs);
                let mut code = code1 + &code2;
                code.push_str(&format!(
                    "if ({}.type != TYPE_INT) return ERR_TYPE;\n",
                    val1
                ));
                code.push_str(&format!(
                    "if ({}.type != TYPE_INT) return ERR_TYPE;\n",
                    val2
                ));
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
                let val = format!(
                    "(Value){{.type = TYPE_INT, .u = {{.int_val = {}.u.int_val {} {}.u.int_val}}}}",
                    val1, op_str, val2
                );
                (code, val)
            }
            Expression::Unary(u) => {
                let (code, val) = self.generate_expr(&u.operand);
                let mut c = code;
                c.push_str(&format!("if ({}.type != TYPE_INT) return ERR_TYPE;\n", val));
                let op_str = match u.op {
                    UnaryOp::Not => "!",
                };
                let res = format!(
                    "(Value){{.type = TYPE_INT, .u = {{.int_val = {}{}.u.int_val}}}}",
                    op_str, val
                );
                (c, res)
            }
            Expression::StackRef(e) => {
                let (code, val) = self.generate_expr(e);
                let mut code = code;
                code.push_str(&format!("off = {};\n", val));
                code.push_str("if (off.type != TYPE_INT) return ERR_TYPE;\n");
                code.push_str("off_val = off.u.int_val;\n");
                code.push_str("if (off_val < 0 || off_val >= sp) return ERR_BOUNDS;\n");
                let val = "stack[sp - 1 - off_val]".to_string();
                (code, val)
            }
            Expression::MemoryRef(e) => {
                let (code, val) = self.generate_expr(e);
                let mut code = code;
                code.push_str(&format!("mem_off = {};\n", val));
                code.push_str("if (mem_off.type != TYPE_INT) return ERR_TYPE;\n");
                code.push_str("mem_off_val = mem_off.u.int_val;\n");
                code.push_str(
                    "if (mem_off_val < 0 || mem_off_val >= MEMORY_SIZE) return ERR_MEMORY;\n",
                );
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
            defs.push_str(&format!("Value v_{};\n", identifier));
        }

        defs
    }

    fn is_gadget(&self, id: &str) -> bool {
        self.ast.gadgets.iter().any(|g| g.name == id)
    }
}
