use crate::ast::*;

use super::*;

pub fn builtin_function(call: &FunCall) -> Option<Vec<SsmInstruction>> {
    let mut instructions = vec![];

    match call.name.0.as_str() {
        "print" => {
            if call.args.len() == 1 {
                match call.args[0].expr_type.as_ref() {
                    Some(Type::Int) | Some(Type::Bool) => {
                        instructions.push(SsmInstruction::Trap(0))
                    }
                    Some(Type::Char) => instructions.push(SsmInstruction::Trap(1)),
                    t => {
                        log::warn!("Printing {:?} types is currently not supported.", t);
                        return Some(vec![]);
                    }
                }

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
