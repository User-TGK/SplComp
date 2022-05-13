use crate::ast::*;

use super::*;

fn print_instructions(t: &Type, prefix_gen: &mut ssm::LabelPrefixGenerator) -> Vec<SsmInstruction> {
    let mut instructions = vec![];

    match t {
        Type::Int | Type::Bool => {
            instructions.push(SsmInstruction::Trap(0));
        }
        Type::Char => {
            instructions.push(SsmInstruction::Trap(1));
        }
        Type::List(t) => {
            let label_start = prefix_gen.new_prefix() + "print_list_begin";
            let last_char = prefix_gen.new_prefix() + "print_list_last";
            let label_end = prefix_gen.new_prefix() + "print_list_end";

            instructions.push(SsmInstruction::Str(String::from("R6")));
            instructions.push(SsmInstruction::Link(2));
            instructions.push(SsmInstruction::Ldr(String::from("R6")));
            instructions.push(SsmInstruction::Stl(1));

            instructions.push(SsmInstruction::Ldc('['.into()));
            instructions.push(SsmInstruction::Trap(1));

            instructions.push(SsmInstruction::Label(label_start.clone()));

            // Store the tail in local var 2
            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Lda(0));
            instructions.push(SsmInstruction::Stl(2));

            // Check whether the list is empty
            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Ldc(ssm::NULL_PTR));
            instructions.push(SsmInstruction::Eq);
            instructions.push(SsmInstruction::Brt(label_end.clone()));

            // Check whether this is the last element
            instructions.push(SsmInstruction::Ldl(2));
            instructions.push(SsmInstruction::Ldc(ssm::NULL_PTR));
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

            instructions.push(SsmInstruction::Bra(label_start));

            instructions.push(SsmInstruction::Label(last_char));
            instructions.push(SsmInstruction::Ldl(1));
            instructions.push(SsmInstruction::Lda(-1));
            instructions.append(&mut print_instructions(t, prefix_gen));

            instructions.push(SsmInstruction::Label(label_end));
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

pub fn builtin_function(
    call: &FunCall,
    prefix_gen: &mut ssm::LabelPrefixGenerator,
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
            instructions.push(SsmInstruction::Ldc(ssm::NULL_PTR));
            instructions.push(SsmInstruction::Eq);

            Some(instructions)
        }

        _ => None,
    }
}
