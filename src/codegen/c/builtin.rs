use crate::ast::*;

use super::*;

use pretty_trait::{JoinExt, Newline, Pretty};

fn print_type_list(t: &Type, tail: String) -> String {
    match t {
        Type::Int => format!("list_node_new(Int, {})", tail),
        Type::Char => format!("list_node_new(Char, {})", tail),
        Type::Bool => format!("list_node_new(Bool, {})", tail),
        Type::List(t1) => format!("list_node_new(List, {})", print_type_list(t1, tail)),
        Type::Tuple(t1, t2) => {
            let tail_t1 = print_type_list(t2, String::from(tail));

            format!("list_node_new(Tuple, {})", print_type_list(t1, tail_t1))
        }
        _ => unreachable!(),
    }
}

pub fn builtin_function(call: &FunCall, env: &mut CEnv) -> Option<Box<dyn Pretty>> {
    match call.name.0.as_str() {
        "print" => {
            if call.args.len() == 1 {
                let t = call.args[0].expr_type.as_ref().unwrap();
                let types = print_type_list(t, String::from("NULL"));

                Some(Box::new(
                    "print((intptr_t) "
                        .join(call.args[0].expr.to_c(env))
                        .join(", ")
                        .join(types)
                        .join(");")
                        .join(Newline)
                        .join("printf(\"\\n\");"),
                ))
            } else {
                None
            }
        }
        _ => None,
    }
}
