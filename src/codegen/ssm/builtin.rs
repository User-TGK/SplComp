use crate::ast::*;

use super::*;

fn print_instructions(t: &Type, prefix_gen: &mut LabelPrefixGenerator) -> Vec<SsmInstruction> {
    let mut instructions = vec![];

    match t {
        Type::Int | Type::Bool => {
            instructions.push(SsmInstruction::Trap(0));
        }
        Type::Char => {
            instructions.push(SsmInstruction::Trap(1));
        }
        Type::List(t) => {
            let list_begin = prefix_gen.new_prefix() + "print_list_begin";
            let last_char = prefix_gen.new_prefix() + "print_list_last";
            let list_end = prefix_gen.new_prefix() + "print_list_end";

            instructions.push(SsmInstruction::Str(String::from("R6")));
            instructions.push(SsmInstruction::Link(2));
            instructions.push(SsmInstruction::Ldr(String::from("R6")));
            instructions.push(SsmInstruction::Stl(1));

            instructions.push(SsmInstruction::Ldc('['.into()));
            instructions.push(SsmInstruction::Trap(1));

            instructions.push(SsmInstruction::Label(list_begin.clone()));

            // Store the tail in local var 2
            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Lda(0));
            instructions.push(SsmInstruction::Stl(2));

            // Check whether the list is empty
            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Ldc(NULL_PTR));
            instructions.push(SsmInstruction::Eq);
            instructions.push(SsmInstruction::Brt(list_end.clone()));

            // Check whether this is the last element
            instructions.push(SsmInstruction::Ldl(2));
            instructions.push(SsmInstruction::Ldc(NULL_PTR));
            instructions.push(SsmInstruction::Eq);
            instructions.push(SsmInstruction::Brt(last_char.clone()));

            // Not yet the last one
            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Lda(-1));
            instructions.append(&mut print_instructions(t, prefix_gen));
            instructions.push(SsmInstruction::Ldc(','.into()));
            instructions.push(SsmInstruction::Trap(1));

            instructions.push(SsmInstruction::Ldl(2));
            instructions.push(SsmInstruction::Stl(1));

            instructions.push(SsmInstruction::Bra(list_begin));

            instructions.push(SsmInstruction::Label(last_char));
            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Lda(-1));
            instructions.append(&mut print_instructions(t, prefix_gen));

            instructions.push(SsmInstruction::Label(list_end));
            instructions.push(SsmInstruction::Ldc(']'.into()));
            instructions.push(SsmInstruction::Trap(1));

            instructions.push(SsmInstruction::Unlink)
        }
        Type::Tuple(t1, t2) => {
            // Use scratch register to temporarily store the pointer to the tuple in R6
            // Create a new stack frame afterwards, retrieve the tuple address from R6
            // and store in the local variable of the frame.
            instructions.push(SsmInstruction::Str(String::from("R6")));
            instructions.push(SsmInstruction::Link(1));
            instructions.push(SsmInstruction::Ldr(String::from("R6")));
            instructions.push(SsmInstruction::Stl(1));

            instructions.push(SsmInstruction::Ldc('('.into()));
            instructions.push(SsmInstruction::Trap(1));

            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Lda(-1));
            instructions.append(&mut print_instructions(t1, prefix_gen));

            instructions.push(SsmInstruction::Ldc(','.into()));
            instructions.push(SsmInstruction::Trap(1));

            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Lda(0));
            instructions.append(&mut print_instructions(t2, prefix_gen));

            instructions.push(SsmInstruction::Ldc(')'.into()));
            instructions.push(SsmInstruction::Trap(1));

            instructions.push(SsmInstruction::Unlink)
        }
        _ => {
            log::warn!("Print called with type {:?}", t);
        }
    }

    instructions
}

pub fn equals_instructions(t: &Type, prefix_gen: &mut LabelPrefixGenerator) -> Vec<SsmInstruction> {
    let mut instructions = vec![];

    match t {
        Type::Int | Type::Bool | Type::Char => {
            instructions.push(SsmInstruction::Eq);
            instructions.push(SsmInstruction::Str(RR.into()));
        }
        Type::List(t) => {
            let list_begin = prefix_gen.new_prefix() + "equals_list_begin";
            let list_end = prefix_gen.new_prefix() + "list_end";
            let unknown_list_end = prefix_gen.new_prefix() + "unknown_list_end";

            instructions.push(SsmInstruction::Str(String::from("R6")));
            instructions.push(SsmInstruction::Str(String::from("R7")));

            instructions.push(SsmInstruction::Link(4));
            instructions.push(SsmInstruction::Ldr(String::from("R6")));
            instructions.push(SsmInstruction::Stl(1));
            instructions.push(SsmInstruction::Ldr(String::from("R7")));
            instructions.push(SsmInstruction::Stl(3));

            instructions.push(SsmInstruction::Label(list_begin.clone()));

            // Check whether either list is empty
            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Ldc(NULL_PTR));
            instructions.push(SsmInstruction::Eq);
            instructions.push(SsmInstruction::Ldl(3));
            instructions.push(SsmInstruction::Ldc(NULL_PTR));
            instructions.push(SsmInstruction::Eq);
            instructions.push(SsmInstruction::Or);

            // Either was empty, move towards the end
            instructions.push(SsmInstruction::Brt(unknown_list_end.clone()));

            // Store the tail in local var 2 and 4
            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Lda(0));
            instructions.push(SsmInstruction::Stl(2));
            instructions.push(SsmInstruction::Ldl(3));
            instructions.push(SsmInstruction::Lda(0));
            instructions.push(SsmInstruction::Stl(4));

            // Compare current element
            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Lda(-1));
            instructions.push(SsmInstruction::Ldl(3));
            instructions.push(SsmInstruction::Lda(-1));

            instructions.append(&mut equals_instructions(t, prefix_gen));
            instructions.push(SsmInstruction::Ldr(RR.into()));
            instructions.push(SsmInstruction::Brf(list_end.clone()));

            // Set the tail to head
            instructions.push(SsmInstruction::Ldl(2));
            instructions.push(SsmInstruction::Stl(1));
            instructions.push(SsmInstruction::Ldl(4));
            instructions.push(SsmInstruction::Stl(3));

            instructions.push(SsmInstruction::Bra(list_begin));

            // Check if both are empty. In that case we have equal.
            instructions.push(SsmInstruction::Label(unknown_list_end));
            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Lda(0));
            instructions.push(SsmInstruction::Ldl(3));
            instructions.push(SsmInstruction::Lda(0));
            instructions.push(SsmInstruction::Eq);
            instructions.push(SsmInstruction::Str(RR.into()));
            instructions.push(SsmInstruction::Bra(list_end.clone()));

            instructions.push(SsmInstruction::Label(list_end));
            instructions.push(SsmInstruction::Unlink)
        }
        Type::Tuple(t1, t2) => {
            let tuple_end = prefix_gen.new_prefix() + "tuple_end";

            instructions.push(SsmInstruction::Str(String::from("R6")));
            instructions.push(SsmInstruction::Str(String::from("R7")));

            instructions.push(SsmInstruction::Link(2));
            instructions.push(SsmInstruction::Ldr(String::from("R6")));
            instructions.push(SsmInstruction::Stl(1));
            instructions.push(SsmInstruction::Ldr(String::from("R7")));
            instructions.push(SsmInstruction::Stl(2));

            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Lda(-1));
            instructions.push(SsmInstruction::Ldl(2));
            instructions.push(SsmInstruction::Lda(-1));
            instructions.append(&mut equals_instructions(t1, prefix_gen));

            instructions.push(SsmInstruction::Ldr(RR.into()));
            instructions.push(SsmInstruction::Brf(tuple_end.clone()));

            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Lda(0));
            instructions.push(SsmInstruction::Ldl(2));
            instructions.push(SsmInstruction::Lda(0));
            instructions.append(&mut equals_instructions(t2, prefix_gen));

            instructions.push(SsmInstruction::Label(tuple_end));
            instructions.push(SsmInstruction::Unlink)
        }
        _ => {
            log::warn!("== called with type {:?}", t);
        }
    }

    instructions
}

pub fn builtin_function(
    call: &FunCall,
    prefix_gen: &mut LabelPrefixGenerator,
) -> Option<Vec<SsmInstruction>> {
    let mut instructions = vec![];

    match call.name.0.as_str() {
        "print" => {
            if call.args.len() == 1 {
                instructions =
                    print_instructions(call.args[0].expr_type.as_ref().unwrap(), prefix_gen);

                instructions.push(SsmInstruction::Ldc('\n' as u32));
                instructions.push(SsmInstruction::Trap(1));

                Some(instructions)
            } else {
                None
            }
        }

        "isEmpty" => {
            instructions.push(SsmInstruction::Ldc(NULL_PTR));
            instructions.push(SsmInstruction::Eq);

            Some(instructions)
        }

        _ => None,
    }
}
