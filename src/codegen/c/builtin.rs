use crate::ast::*;

use super::*;

use pretty_trait::{JoinExt, Newline, Pretty};

pub fn print_type_list(t: &Type, tail: String) -> Result<String, String> {
    match t {
        Type::Int => Ok(format!("list_node_new(Int, {})", tail)),
        Type::Char => Ok(format!("list_node_new(Char, {})", tail)),
        Type::Bool => Ok(format!("list_node_new(Bool, {})", tail)),
        Type::List(t1) => Ok(format!(
            "list_node_new(List, {})",
            print_type_list(t1, tail)?
        )),
        Type::Tuple(t1, t2) => {
            let tail_t1 = print_type_list(t2, String::from(tail))?;

            Ok(format!(
                "list_node_new(Tuple, {})",
                print_type_list(t1, tail_t1)?
            ))
        }
        Type::Var(v) => Err(format!(
            "Illegal call to overloaded function ('print'/'equal') with generic type '{}'",
            v.0
        )),
        _ => unreachable!(),
    }
}

pub fn builtin_function(call: &FunCall, env: &mut CEnv) -> Result<Option<Box<dyn Pretty>>, String> {
    match call.name.0.as_str() {
        "print" => {
            if call.args.len() == 1 {
                let t = call.args[0].expr_type.as_ref().unwrap();
                let types = print_type_list(t, String::from("NULL"))?;

                Ok(Some(Box::new(
                    "print((intptr_t) "
                        .join(call.args[0].to_c(env)?)
                        .join(", ")
                        .join(types)
                        .join(");")
                        .join(Newline)
                        .join("printf(\"\\n\");"),
                )))
            } else {
                Ok(None)
            }
        }
        _ => Ok(None),
    }
}
