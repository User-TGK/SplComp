use crate::ast::*;

use super::builtin::*;

use num_bigint::BigUint;
use num_traits::cast::ToPrimitive;

use core::panic;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

/// Start heap register. Points towards the start of the heap.
/// Globals have an offset relative to this pointer.
const SHP: &'static str = "R5";
/// Return register.
pub const RR: &'static str = "RR";
/// NULL pointer used to represent an empty list.
pub const NULL_PTR: u32 = 0x00;

macro_rules! unary_expr(
    ($e:expr, $env:expr, $heap_offset:expr, $prefix_gen:expr, $instruction:expr) => (
        $e.instructions($env, $heap_offset, $prefix_gen)
            .into_iter()
            .chain(vec![$instruction])
            .collect()
    )
);

macro_rules! bin_exp (
    ($e1:expr, $e2:expr, $env:expr, $heap_offset:expr, $prefix_gen:expr, $instruction:expr) => (
        $e1.instructions($env, $heap_offset, $prefix_gen)
            .into_iter()
            .chain($e2.instructions($env, $heap_offset, $prefix_gen).into_iter())
            .chain(vec![$instruction])
            .collect()
    )
);

#[derive(Default)]
pub struct LabelPrefixGenerator {
    counter: usize,
}

impl LabelPrefixGenerator {
    pub fn new_prefix(&mut self) -> String {
        let var = format!("pre_{}_", self.counter);
        self.counter += 1;

        var
    }
}

pub trait SsmInstructions {
    fn instructions(
        &self,
        env: &mut LocationEnv,
        heap_offset: &mut i32,
        prefix_gen: &mut LabelPrefixGenerator,
    ) -> Vec<SsmInstruction>;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SsmInstruction {
    /// A label is an identifier indicating a position in the code.
    /// When loading, the code location of a label is calculated (called resolution).
    /// This is done in the user interface of the program and after loading labels are not kept consistent (when adding new instructions for example).
    Label(String),
    /// Load Constant. Pushes the inline constant on the stack.
    Ldc(u32),
    /// Load from Stack. Pushes a value relative to the top of the stack.
    Lds(i32),
    /// Load Multiple from Stack. Pushes values relative to the top of the stack. Same as single load variant but second inline parameter is size.
    Ldms(i32, i32),
    /// Store into Stack. Pops a value from the stack and stores it in a location relative to the top of the stack.
    Sts(i32),
    /// Store Multiple into Stack. Pops values from the stack and stores it in a location relative to the top of the stack. Same as single store variant but second inline parameter is size.
    Stms(i32, i32),
    /// Load Stack Address. Pushes the address of a value relative to the stackpointer.
    Ldsa(i32),
    /// Load Local. Pushes a value relative to the markpointer.
    Ldl(i32),
    /// Load Multiple Local. Pushes values relative to the markpointer. Same as single load variant but second inline parameter is size.
    Ldml(i32, i32),
    /// Store Local. Pops a value from the stack and stores it in a location relative to the markpointer.
    Stl(i32),
    /// Store Multiple Local. Pops values from the stack and stores it in a location relative to the markpointer. Same as single store variant but second inline parameter is size.
    Stml(i32, i32),
    /// Load Local Address. Pushes the address of a value relative to the markpointer.
    Ldla(i32),
    /// Load via Address. Dereferencing. Pushes the value pointed to by the value at the top of the stack. The pointer value is offset by a constant offset.
    Lda(i32),
    /// Load Multiple via Address. Pushes values relative to by the value at the top of the stack. Same as single load variant but second inline parameter is size.
    Ldma(i32, i32),
    /// Load Address of Address. Add a constant offset to the address on top of the stack.
    Ldaa(i32),
    /// Store via Address. Pops 2 values from the stack and stores the second popped value in the location pointed to by the first. The pointer value is offset by a constant offset.
    Sta(i32),
    /// Store Multiple via Address. Pops values from the stack and stores it in a location relative to the value at the top of the stack. Same as single store variant but second inline parameter is size.
    Stma(i32, i32),
    /// Load Register. Pushes a value from a register. Registers 0, 1, 2, 3 and 4 are called PC (programcounter), SP (stackpointer), MP (markpointer), HP (heappointer) and RR (return register) respectively.
    Ldr(String),
    /// Load Register from Register. Copy the content of the second register to the first. Does not affect the stack.
    Ldrr(String, String),
    /// Store Register. Pops a value from the stack and stores it in the specified register. See also ldr.
    Str(String),
    /// Swap values. Swaps the 2 topmost values on the stack.
    Swp,
    /// Swap Register. Swaps the content of a register with the top of the stack.
    Swpr(String),
    /// Swap 2 Registers. Swaps the content of a register with another register.
    Swprr(String, String),
    /// Adjust Stack. Adjusts the stackpointer with fixed amount.
    Ajs(i32),
    // Addition. Replaces 2 top stack values with the addition of those values.
    Add,
    /// Multiplication. Replaces 2 top stack values with the multiplication of those values.
    Mul,
    /// Substraction. Replaces 2 top stack values with the subtraction of those values.
    Sub,
    /// Division. Replaces 2 top stack values with the division of those values.
    Div,
    /// Division. Replaces 2 top stack values with the modulo of those values.
    Mod,
    /// And. Replaces 2 top stack values with the bitwise and of those values.
    And,
    /// Or. Replaces 2 top stack values with the bitwise or of those values.
    Or,
    /// Exclusive Or. Replaces 2 top stack values with the bitwise exclusive or of those values.
    Xor,
    /// Test for equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as -1 (0xFFFFFFFF). Used in combination with brf. This is a variant of cmp combined with beq.
    Eq,
    /// Test for not equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as -1 (0xFFFFFFFF). Used in combination with brf. This is a variant of cmp combined with bne.
    Ne,
    /// Test for less then. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as -1 (0xFFFFFFFF). Used in combination with brf. This is a variant of cmp combined with blt.
    Lt,
    /// Test for less or equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as -1 (0xFFFFFFFF). Used in combination with brf. This is a variant of cmp combined with ble.
    Le,
    /// Test for greater then. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as -1 (0xFFFFFFFF). Used in combination with brf. This is a variant of cmp combined with bgt.
    Gt,
    /// Test for greater or equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as -1 (0xFFFFFFFF). Used in combination with brf. This is a variant of cmp combined with bge.
    Ge,
    /// Negation. Replaces top stack values with the (integer) negative of the value.
    Neg,
    /// Not. Replaces top stack values with the bitwise complement of the value.
    Not,
    /// Branch to subroutine. Pushes the PC on the stack and jumps to the subroutine.
    Bsr(String),
    /// Branch Allways. Jumps to the destination. Replaces the PC with the destination address.
    Bra(String),
    /// Branch on False. If a False value is on top of the stack, jump to the destination.
    Brf(String),
    /// Branch on True. If a True value is on top of the stack, jump to the destination.
    Brt(String),
    /// Jump to subroutine. Pops a destination from the stack, pushes the PC on the stack and jumps to the destination.
    Jsr(String),
    /// Return from subroutine. Pops a previously pushed PC from the stack and jumps to it.
    Ret,
    /// Reserve memory for locals. Convenience instruction combining the push of MP and the adjustment of the SP.
    Link(u32),
    /// Free memory for locals. Convenience instruction combining the push of MP and the adjustment of the SP.
    Unlink,
    /// No operation. Well, guess what...
    Nop,
    /// Halt execution. Machine stops executing instructions.
    Halt,
    /// Trap to environment function. Trap invokes a systemcall determined by its argument.
    Trap(i32),
    /// Annotate. A meta instruction (not producing code), annotating the stack display in the user interface with text and color. Annote takes 5 arguments, (1) a register name, (2) a low offset w.r.t. the register (used as starting point for annotating), (3) a high offset, (4) a color, (5) text. Color can be one of {black, blue, cyan, darkGray, gray, green, lightGray, magenta, orange, pink, red, yellow}. Text including spaces need to be enclosed in double quotes. The annote instruction is tied to the preceding (non-meta) instruction and will be performed immediately after the execution of that instruction.
    Annotate(String, i32, i32, String, String),
    /// Load from Heap. Pushes a value pointed to by the value at the top of the stack. The pointer value is offset by a constant offset.
    Ldh(i32),
    /// Load Multiple from Heap. Pushes values pointed to by the value at the top of the stack. The pointer value is offset by a constant offset. Same as single load variant but the second inline parameter is size.
    Ldmh(i32, i32),
    /// Store into Heap. Pops 1 value from the stack and stores it into the heap. Pushes the heap address of that value on the stack.
    Sth,
    /// Store Multiple into Heap. Pops values from the stack and stores it into the heap, retaining the order of the values. Same as single store variant but the inline parameter is size. Pushes the heap address of the last value on the stack.
    Stmh(i32),
}

impl From<SsmInstruction> for String {
    fn from(instruction: SsmInstruction) -> String {
        match instruction {
            SsmInstruction::Label(l) => format!("{}:", l),
            SsmInstruction::Ldc(c) => format!("ldc {}", c),
            SsmInstruction::Lds(o) => format!("lds {}", o),
            SsmInstruction::Ldms(o, s) => format!("ldms {} {}", o, s),
            SsmInstruction::Sts(o) => format!("sts {}", o),
            SsmInstruction::Stms(o, s) => format!("stms {} {}", o, s),
            SsmInstruction::Ldsa(o) => format!("ldsa {}", o),
            SsmInstruction::Ldl(o) => format!("ldl {}", o),
            SsmInstruction::Ldml(o, s) => format!("ldml {} {}", o, s),
            SsmInstruction::Stl(o) => format!("stl {}", o),
            SsmInstruction::Stml(o, s) => format!("stml {} {}", o, s),
            SsmInstruction::Ldla(o) => format!("ldla {}", o),
            SsmInstruction::Lda(o) => format!("lda {}", o),
            SsmInstruction::Ldma(o, s) => format!("ldma {} {}", o, s),
            SsmInstruction::Ldaa(o) => format!("ldaa {}", o),
            SsmInstruction::Sta(o) => format!("sta {}", o),
            SsmInstruction::Stma(o, s) => format!("stma {} {}", o, s),
            SsmInstruction::Ldr(r) => format!("ldr {}", r),
            SsmInstruction::Ldrr(r1, r2) => format!("ldrr {} {}", r1, r2),
            SsmInstruction::Str(r) => format!("str {}", r),
            SsmInstruction::Swp => String::from("swp"),
            SsmInstruction::Swpr(r) => format!("swpr {}", r),
            SsmInstruction::Swprr(r1, r2) => format!("swprr {} {}", r1, r2),
            SsmInstruction::Ajs(o) => format!("ajs {}", o),
            SsmInstruction::Add => String::from("add"),
            SsmInstruction::Mul => String::from("mul"),
            SsmInstruction::Sub => String::from("sub"),
            SsmInstruction::Div => String::from("div"),
            SsmInstruction::Mod => String::from("mod"),
            SsmInstruction::And => String::from("and"),
            SsmInstruction::Or => String::from("or"),
            SsmInstruction::Xor => String::from("xor"),
            SsmInstruction::Eq => String::from("eq"),
            SsmInstruction::Ne => String::from("ne"),
            SsmInstruction::Lt => String::from("lt"),
            SsmInstruction::Le => String::from("le"),
            SsmInstruction::Gt => String::from("gt"),
            SsmInstruction::Ge => String::from("ge"),
            SsmInstruction::Neg => String::from("neg"),
            SsmInstruction::Not => String::from("not"),
            SsmInstruction::Bsr(s) => format!("bsr {}", s),
            SsmInstruction::Bra(s) => format!("bra {}", s),
            SsmInstruction::Brf(s) => format!("brf {}", s),
            SsmInstruction::Brt(s) => format!("brt {}", s),
            SsmInstruction::Jsr(s) => format!("jsr {}", s),
            SsmInstruction::Ret => String::from("ret"),
            SsmInstruction::Link(n) => format!("link {}", n),
            SsmInstruction::Unlink => String::from("unlink"),
            SsmInstruction::Nop => String::from("nop"),
            SsmInstruction::Halt => String::from("halt"),
            SsmInstruction::Trap(c) => format!("trap {}", c),
            SsmInstruction::Annotate(r, l, h, c, t) => format!("{} {} {} {} \"{}\"", r, l, h, c, t),
            SsmInstruction::Ldh(o) => format!("ldh {}", o),
            SsmInstruction::Ldmh(o, s) => format!("ldmh {} {}", o, s),
            SsmInstruction::Sth => String::from("sth"),
            SsmInstruction::Stmh(s) => format!("stmh {}", s),
        }
    }
}

#[derive(Clone, Debug)]
pub enum SsmLocation {
    /// Offset from the start of the heap.
    Heap(i32),
    /// Offset from the MP.
    Stack(i32),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SsmScope {
    Global,
    LocalArgs,
    Local,
}

#[derive(Clone, Debug, Default)]
pub struct LocationEnv(HashMap<(Id, SsmScope), SsmLocation>);

impl LocationEnv {
    /// Pointers to tuples and lists point towards the right part of the
    /// structure (i.e. second tuple component and list tail) due to the
    /// `stmh` instruction returning the address of the last cell data was
    /// stored. This function provides the appropriate offsets when requiring
    /// field access (e.g. for `sta`, `lda`)
    pub fn field_offset(field: &Field) -> i32 {
        match field {
            Field::Hd | Field::Fst => -1,
            Field::Tl | Field::Snd => 0,
        }
    }

    /// Returns the instructions to load a variable on the stack.
    pub fn load(&self, name: Id) -> Vec<SsmInstruction> {
        match (
            self.get(&(name.clone(), SsmScope::Local)),
            self.get(&(name.clone(), SsmScope::LocalArgs)),
            self.get(&(name.clone(), SsmScope::Global)),
        ) {
            (Some(a), _, _) | (_, Some(a), _) | (_, _, Some(a)) => match a {
                SsmLocation::Stack(offset) => vec![SsmInstruction::Ldl(*offset)],
                SsmLocation::Heap(offset) => vec![
                    SsmInstruction::Ldr(SHP.into()),
                    SsmInstruction::Ldh(*offset),
                ],
            },

            // This should be unreachable. But just in case, lets print an error.
            (None, None, None) => {
                log::error!("MISSING IDENTIFIER '{}' IN ENV.", name);
                vec![]
            }
        }
    }

    pub fn address(&self, name: Id) -> SsmLocation {
        match (
            self.get(&(name.clone(), SsmScope::Local)),
            self.get(&(name.clone(), SsmScope::LocalArgs)),
            self.get(&(name.clone(), SsmScope::Global)),
        ) {
            (Some(a), _, _) => a.clone(),
            (_, Some(a), _) => a.clone(),
            (_, _, Some(a)) => a.clone(),
            (None, None, None) => {
                panic!(
                    "This should be unreachable. '{}' missing from environment.",
                    name
                );
            }
        }
    }

    /// Removes all local identifiers from the environment.
    pub fn remove_locals(&mut self) {
        self.0.retain(|(_, scope), _| match scope {
            SsmScope::Global => true,
            _ => false,
        });
    }
}

impl Deref for LocationEnv {
    type Target = HashMap<(Id, SsmScope), SsmLocation>;
    fn deref(&self) -> &HashMap<(Id, SsmScope), SsmLocation> {
        &self.0
    }
}
impl DerefMut for LocationEnv {
    fn deref_mut(&mut self) -> &mut HashMap<(Id, SsmScope), SsmLocation> {
        &mut self.0
    }
}

/// Copied from https://stackoverflow.com/a/50485697
/// Not sure if this is the desired behavior, but it provdes a wait for this to work
fn truncate_biguint_to_u32(a: &BigUint) -> u32 {
    use std::u32;
    let mask = BigUint::from(u32::MAX);
    (a & mask).to_u32().unwrap()
}

impl SsmInstructions for Program {
    fn instructions(
        &self,
        env: &mut LocationEnv,
        heap_offset: &mut i32,
        prefix_gen: &mut LabelPrefixGenerator,
    ) -> Vec<SsmInstruction> {
        // Load heap pointer to R5 so we know the start.
        let mut instructions = vec![
            SsmInstruction::Ldr(String::from("HP")),
            SsmInstruction::Str(SHP.into()),
        ];

        // Store globals on the heap.
        for v in &self.var_decls {
            instructions.append(&mut v.value.instructions(env, heap_offset, prefix_gen));
            instructions.push(SsmInstruction::Sth);
            instructions.push(SsmInstruction::Ajs(-1));

            env.insert(
                (v.name.clone(), SsmScope::Global),
                SsmLocation::Heap(*heap_offset),
            );
            *heap_offset = *heap_offset + 1;
        }

        // Invoke starting point main.
        instructions.push(SsmInstruction::Bsr(String::from("main")));
        instructions.push(SsmInstruction::Halt);

        for f in &self.fun_decls {
            instructions.append(&mut f.instructions(env, heap_offset, prefix_gen));

            env.remove_locals();
        }

        instructions.push(SsmInstruction::Halt);
        instructions
    }
}

impl SsmInstructions for FunDecl {
    fn instructions(
        &self,
        env: &mut LocationEnv,
        heap_offset: &mut i32,
        prefix_gen: &mut LabelPrefixGenerator,
    ) -> Vec<SsmInstruction> {
        let mut instructions = vec![];

        instructions.push(SsmInstruction::Label(self.name.0.clone()));
        instructions.push(SsmInstruction::Link(self.var_decls.len() as u32));

        let mut offset_args = -1 * self.params.len() as i32 - 1;

        for p in &self.params {
            env.insert(
                (p.clone(), SsmScope::LocalArgs),
                SsmLocation::Stack(offset_args),
            );
            offset_args += 1;
        }

        let mut offset_local = 1;

        for v in &self.var_decls {
            instructions.append(&mut v.value.instructions(env, heap_offset, prefix_gen));
            instructions.push(SsmInstruction::Stl(offset_local));
            env.insert(
                (v.name.clone(), SsmScope::Local),
                SsmLocation::Stack(offset_local),
            );

            offset_local += 1;
        }

        for s in &self.statements {
            instructions.append(&mut s.instructions(env, heap_offset, prefix_gen));
        }

        instructions
    }
}

impl SsmInstructions for Statement {
    fn instructions(
        &self,
        env: &mut LocationEnv,
        heap_offset: &mut i32,
        prefix_gen: &mut LabelPrefixGenerator,
    ) -> Vec<SsmInstruction> {
        match self {
            Statement::If(i) => i.instructions(env, heap_offset, prefix_gen),
            Statement::While(w) => w.instructions(env, heap_offset, prefix_gen),
            Statement::Assign(a) => a.instructions(env, heap_offset, prefix_gen),

            // Can we use the same instructions as for expression funcalls?
            Statement::FunCall(f) => f.instructions(env, heap_offset, prefix_gen),

            Statement::Return(None) => {
                vec![SsmInstruction::Unlink, SsmInstruction::Ret]
            }
            Statement::Return(Some(r)) => {
                let mut instructions = r.instructions(env, heap_offset, prefix_gen);
                instructions.push(SsmInstruction::Str(RR.into()));
                instructions.push(SsmInstruction::Unlink);
                instructions.push(SsmInstruction::Ret);

                instructions
            }
        }
    }
}

impl SsmInstructions for If {
    fn instructions(
        &self,
        env: &mut LocationEnv,
        heap_offset: &mut i32,
        prefix_gen: &mut LabelPrefixGenerator,
    ) -> Vec<SsmInstruction> {
        let mut instructions = self.cond.instructions(env, heap_offset, prefix_gen);
        let prefix = prefix_gen.new_prefix();

        let true_label = prefix.clone() + "true_banch";
        let false_label = prefix.clone() + "false_branch";
        let finished_label = prefix + "end_if_branch";

        if self.if_false.is_empty() {
            instructions.push(SsmInstruction::Brf(finished_label.clone()));
        } else {
            instructions.push(SsmInstruction::Brf(false_label.clone()));
        }

        instructions.push(SsmInstruction::Label(true_label));

        for s in &self.if_true {
            instructions.append(&mut s.instructions(env, heap_offset, prefix_gen));
        }

        instructions.push(SsmInstruction::Bra(finished_label.clone()));

        if !self.if_false.is_empty() {
            instructions.push(SsmInstruction::Label(false_label));
            for s in &self.if_false {
                instructions.append(&mut s.instructions(env, heap_offset, prefix_gen));
            }
        }

        instructions.push(SsmInstruction::Label(finished_label));

        instructions
    }
}

impl SsmInstructions for While {
    fn instructions(
        &self,
        env: &mut LocationEnv,
        heap_offset: &mut i32,
        prefix_gen: &mut LabelPrefixGenerator,
    ) -> Vec<SsmInstruction> {
        let label = prefix_gen.new_prefix() + "while";
        let label_end = prefix_gen.new_prefix() + "while_end";

        let mut instructions = vec![SsmInstruction::Label(label.clone())];

        instructions.append(&mut self.cond.instructions(env, heap_offset, prefix_gen));
        instructions.push(SsmInstruction::Brf(label_end.clone()));

        for i in &self.body {
            instructions.append(&mut i.instructions(env, heap_offset, prefix_gen));
        }

        instructions.push(SsmInstruction::Bra(label));
        instructions.push(SsmInstruction::Label(label_end));

        instructions
    }
}

impl SsmInstructions for Assign {
    fn instructions(
        &self,
        env: &mut LocationEnv,
        heap_offset: &mut i32,
        prefix_gen: &mut LabelPrefixGenerator,
    ) -> Vec<SsmInstruction> {
        let mut instructions = self.value.instructions(env, heap_offset, prefix_gen);

        if self.target.fields.is_empty() {
            match env.address(self.target.name.clone()) {
                // Assignment to a global variable.
                SsmLocation::Heap(o) => {
                    instructions.push(SsmInstruction::Ldr(SsmInstruction::Sth.into()));
                    instructions.push(SsmInstruction::Sta(o));
                }
                // Assignment to a local variable.
                SsmLocation::Stack(o) => {
                    // Basic value or an entire object: simply overwrite the current local.
                    instructions.push(SsmInstruction::Stl(o));
                }
            }
        } else {
            // Fields: load the objects at the appropriate addresses and only update
            // a part of the object.
            instructions.append(&mut env.load(self.target.name.clone()));

            for f in &self.target.fields[..self.target.fields.len() - 1] {
                instructions.push(SsmInstruction::Lda(LocationEnv::field_offset(f)));
            }

            if let Some(f) = self.target.fields.last() {
                instructions.push(SsmInstruction::Sta(LocationEnv::field_offset(f)));
            }
        }

        instructions
    }
}

impl SsmInstructions for TypedExpr {
    fn instructions(
        &self,
        env: &mut LocationEnv,
        heap_offset: &mut i32,
        prefix_gen: &mut LabelPrefixGenerator,
    ) -> Vec<SsmInstruction> {
        self.expr.instructions(env, heap_offset, prefix_gen)
    }
}

impl SsmInstructions for Expr {
    fn instructions(
        &self,
        env: &mut LocationEnv,
        heap_offset: &mut i32,
        gen: &mut LabelPrefixGenerator,
    ) -> Vec<SsmInstruction> {
        match self {
            Expr::Or(e1, e2) => bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::Or),
            Expr::And(e1, e2) => bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::And),
            Expr::Equals(e1, e2) => bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::Eq),
            Expr::NotEquals(e1, e2) => bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::Ne),
            Expr::Lt(e1, e2) => bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::Lt),
            Expr::Le(e1, e2) => bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::Le),
            Expr::Gt(e1, e2) => bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::Gt),
            Expr::Ge(e1, e2) => bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::Ge),
            Expr::Add(e1, e2) => bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::Add),
            Expr::Sub(e1, e2) => bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::Sub),
            Expr::Mul(e1, e2) => bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::Mul),
            Expr::Div(e1, e2) => bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::Div),
            Expr::Mod(e1, e2) => bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::Mod),
            Expr::UnaryMinus(e) => unary_expr!(e, env, heap_offset, gen, SsmInstruction::Neg),
            Expr::Not(e) => unary_expr!(e, env, heap_offset, gen, SsmInstruction::Not),
            Expr::Atom(a) => a.instructions(env, heap_offset, gen),

            Expr::Cons(e1, e2) => {
                *heap_offset = *heap_offset + 2;
                bin_exp!(e1, e2, env, heap_offset, gen, SsmInstruction::Stmh(2))
            }
        }
    }
}

impl SsmInstructions for Atom {
    fn instructions(
        &self,
        env: &mut LocationEnv,
        heap_offset: &mut i32,
        prefix_gen: &mut LabelPrefixGenerator,
    ) -> Vec<SsmInstruction> {
        match self {
            Atom::IntLiteral(i) => vec![SsmInstruction::Ldc(truncate_biguint_to_u32(i))],

            // False is encoded as 0
            // True as -1 (0xFFFFFFFF).
            Atom::BoolLiteral(b) => {
                vec![match b {
                    true => SsmInstruction::Ldc(0xFFFFFFFF),
                    false => SsmInstruction::Ldc(0x00),
                }]
            }

            Atom::CharLiteral(c) => {
                vec![SsmInstruction::Ldc(c.clone().into())]
            }

            // "abc" => "'a' : 'b' : 'c' : []"
            Atom::StringLiteral(s) => {
                if s.is_empty() {
                    return vec![SsmInstruction::Ldc(NULL_PTR)];
                }

                let mut instructions = vec![];
                for c in s.chars() {
                    instructions.push(SsmInstruction::Ldc(c.into()));
                }
                instructions.push(SsmInstruction::Ldc(NULL_PTR));
                instructions.append(&mut s.chars().map(|_| SsmInstruction::Stmh(2)).collect());
                *heap_offset = *heap_offset + (2 * s.len() as i32);

                instructions
            }
            Atom::FunCall(f) => f.instructions(env, heap_offset, prefix_gen),
            Atom::Variable(v) => v.instructions(env, heap_offset, prefix_gen),
            Atom::EmptyList => vec![SsmInstruction::Ldc(NULL_PTR)],
            Atom::Tuple(e1, e2) => {
                *heap_offset = *heap_offset + 2;

                let mut instructions = e1.instructions(env, heap_offset, prefix_gen);
                instructions.append(&mut e2.instructions(env, heap_offset, prefix_gen));
                instructions.push(SsmInstruction::Stmh(2));

                instructions
            }
        }
    }
}

impl SsmInstructions for FunCall {
    fn instructions(
        &self,
        env: &mut LocationEnv,
        heap_offset: &mut i32,
        prefix_gen: &mut LabelPrefixGenerator,
    ) -> Vec<SsmInstruction> {
        let mut instructions = vec![];

        for a in &self.args {
            instructions.append(&mut a.instructions(env, heap_offset, prefix_gen));
        }

        if let Some(mut builtin_instructions) = builtin_function(&self, prefix_gen) {
            instructions.append(&mut builtin_instructions);
        } else {
            instructions.push(SsmInstruction::Bsr(self.name.0.clone()));
            instructions.push(SsmInstruction::Ajs(-1 * self.args.len() as i32));
            instructions.push(SsmInstruction::Ldr(RR.into()));
        }

        instructions
    }
}

impl SsmInstructions for Variable {
    fn instructions(
        &self,
        env: &mut LocationEnv,
        _heap_offset: &mut i32,
        _prefix_gen: &mut LabelPrefixGenerator,
    ) -> Vec<SsmInstruction> {
        let mut instructions = env.load(self.name.clone());

        for f in &self.fields {
            instructions.push(SsmInstruction::Lda(LocationEnv::field_offset(f)));
        }

        instructions
    }
}
