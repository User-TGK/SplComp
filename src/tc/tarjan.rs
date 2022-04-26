use crate::ast::*;

use super::TypeEnv;

use petgraph::algo::{tarjan_scc, toposort};
use petgraph::graph::Graph;

use std::collections::HashMap;

type DirectedGraph<'a> = Graph<usize, ()>;

/// Trait to preprocess our AST before running type inference.
pub trait Preprocess {
    fn resort_vars(&mut self) -> Result<(), String>;
    fn run_tarjan(&mut self, function_env: &TypeEnv) -> Result<Vec<Vec<usize>>, String>;
}

impl Preprocess for Program {
    /// Resorts all global variables in the AST according to call usage using reverse topological sort.
    fn resort_vars(&mut self) -> Result<(), String> {
        let mut var_graph = DirectedGraph::new();
        let mut var_id_index_map = HashMap::new();

        // Add all nodes
        for (i, v) in self.var_decls.iter().enumerate() {
            var_id_index_map.insert(&v.name, (i, var_graph.add_node(i)));
        }

        // Add all edges
        for v in &self.var_decls {
            let variables = v.value.variables();

            for var in variables {
                var_graph.add_edge(var_id_index_map[&v.name].1, var_id_index_map[&var].1, ());
            }
        }

        let mut indices = vec![];

        // Run topological sorting and iterate in reverse
        match toposort(&var_graph, None) {
            Ok(var_sort) => {
                for v in var_sort.into_iter().rev() {
                    indices.push(var_graph[v]);
                }
            }
            Err(e) => {
                let node_id = e.node_id();

                if let Some(name) = var_id_index_map.iter().find_map(|(key, &val)| {
                    if val == (var_graph[node_id], node_id) {
                        Some(key)
                    } else {
                        None
                    }
                }) {
                    return Err(format!("Global variable {} recursively defined", name));
                } else {
                    // This should never be reached
                    unreachable!();
                }
            }
        }

        sort_by_indices(&mut self.var_decls, indices);

        Ok(())
    }

    fn run_tarjan(&mut self, function_env: &TypeEnv) -> Result<Vec<Vec<usize>>, String> {
        let mut fun_graph = DirectedGraph::new();
        let mut fun_id_index_map = HashMap::new();

        // Add all nodes
        for (i, f) in self.fun_decls.iter().enumerate() {
            fun_id_index_map.insert(&f.name, (i, fun_graph.add_node(i)));
        }

        // Add all edges
        for f in &self.fun_decls {
            let fun_calls = f.statements.function_calls();

            for c in fun_calls {
                if let Some(target_node) = fun_id_index_map.get(&c) {
                    fun_graph.add_edge(fun_id_index_map[&f.name].1, target_node.1, ());
                } else if !function_env.contains_key(&c) {
                    return Err(format!("Undefined function '{}'", c));
                }
            }
        }

        let mut indices = vec![];

        // Run Tarjan's scc algorithm
        let scc = tarjan_scc(&fun_graph);
        let mut scc_indices = vec![];

        // Resort function declarations and resort
        for component in &scc {
            let mut component_indices = vec![];

            for n in component {
                component_indices.push(indices.len());
                indices.push(fun_graph[n.clone()]);
            }
            scc_indices.push(component_indices);
        }

        sort_by_indices(&mut self.fun_decls, indices);

        Ok(scc_indices)
    }
}

/// Helper function to resort a vector by indices.
/// Copied from https://stackoverflow.com/questions/69764803/how-to-sort-a-vector-by-indices-in-rust
pub fn sort_by_indices<T>(data: &mut [T], mut indices: Vec<usize>) {
    for idx in 0..data.len() {
        if indices[idx] != idx {
            let mut current_idx = idx;
            loop {
                let target_idx = indices[current_idx];
                indices[current_idx] = current_idx;
                if indices[target_idx] == target_idx {
                    break;
                }
                data.swap(current_idx, target_idx);
                current_idx = target_idx;
            }
        }
    }
}

trait ContainsVarIdentifier {
    fn variables(&self) -> Vec<Id>;
}

trait ContainsIdentifier {
    fn function_calls(&self) -> Vec<Id>;
}

impl ContainsIdentifier for Vec<Statement> {
    fn function_calls(&self) -> Vec<Id> {
        let mut res = vec![];

        for s in self {
            res.append(&mut s.function_calls());
        }

        res
    }
}

impl ContainsIdentifier for Statement {
    fn function_calls(&self) -> Vec<Id> {
        match self {
            Statement::If(i) => i.function_calls(),
            Statement::While(w) => w.function_calls(),
            Statement::Assign(a) => a.function_calls(),
            Statement::FunCall(f) => f.function_calls(),
            Statement::Return(e) => match e {
                Some(e) => e.function_calls(),
                None => vec![],
            },
        }
    }
}

impl ContainsIdentifier for If {
    fn function_calls(&self) -> Vec<Id> {
        let mut res = vec![];

        res.append(&mut self.cond.function_calls());
        res.append(&mut self.if_true.function_calls());
        res.append(&mut self.if_false.function_calls());

        res
    }
}

impl ContainsIdentifier for While {
    fn function_calls(&self) -> Vec<Id> {
        let mut res = vec![];

        res.append(&mut self.cond.function_calls());
        res.append(&mut self.body.function_calls());

        res
    }
}

impl ContainsIdentifier for Assign {
    fn function_calls(&self) -> Vec<Id> {
        let mut res = vec![];

        res.append(&mut self.value.function_calls());

        res
    }
}

impl ContainsIdentifier for FunCall {
    fn function_calls(&self) -> Vec<Id> {
        let mut res = vec![self.name.clone()];

        for a in &self.args {
            res.append(&mut a.function_calls());
        }

        res
    }
}

impl ContainsVarIdentifier for FunCall {
    fn variables(&self) -> Vec<Id> {
        let mut res = vec![];

        for a in &self.args {
            res.append(&mut a.variables());
        }

        res
    }
}

impl ContainsIdentifier for Expr {
    fn function_calls(&self) -> Vec<Id> {
        match self {
            Expr::Or(e1, e2)
            | Expr::And(e1, e2)
            | Expr::Equals(e1, e2)
            | Expr::NotEquals(e1, e2)
            | Expr::Lt(e1, e2)
            | Expr::Le(e1, e2)
            | Expr::Gt(e1, e2)
            | Expr::Ge(e1, e2)
            | Expr::Add(e1, e2)
            | Expr::Sub(e1, e2)
            | Expr::Mul(e1, e2)
            | Expr::Div(e1, e2)
            | Expr::Mod(e1, e2)
            | Expr::Cons(e1, e2) => e1
                .function_calls()
                .into_iter()
                .chain(e2.function_calls().into_iter())
                .collect(),
            Expr::UnaryMinus(e) | Expr::Not(e) => e.function_calls(),
            Expr::Atom(a) => a.function_calls(),
        }
    }
}

impl ContainsVarIdentifier for Expr {
    fn variables(&self) -> Vec<Id> {
        match self {
            Expr::Or(e1, e2)
            | Expr::And(e1, e2)
            | Expr::Equals(e1, e2)
            | Expr::NotEquals(e1, e2)
            | Expr::Lt(e1, e2)
            | Expr::Le(e1, e2)
            | Expr::Gt(e1, e2)
            | Expr::Ge(e1, e2)
            | Expr::Add(e1, e2)
            | Expr::Sub(e1, e2)
            | Expr::Mul(e1, e2)
            | Expr::Div(e1, e2)
            | Expr::Mod(e1, e2)
            | Expr::Cons(e1, e2) => e1
                .variables()
                .into_iter()
                .chain(e2.variables().into_iter())
                .collect(),
            Expr::UnaryMinus(e) | Expr::Not(e) => e.variables(),
            Expr::Atom(a) => a.variables(),
        }
    }
}

impl ContainsIdentifier for Atom {
    fn function_calls(&self) -> Vec<Id> {
        match self {
            Atom::FunCall(f) => f.function_calls(),
            Atom::Tuple(e1, e2) => e1
                .function_calls()
                .into_iter()
                .chain(e2.function_calls().into_iter())
                .collect(),
            _ => vec![],
        }
    }
}

impl ContainsVarIdentifier for Atom {
    fn variables(&self) -> Vec<Id> {
        match self {
            Atom::FunCall(f) => f.variables(),
            Atom::Variable(v) => vec![v.name.clone()],
            Atom::Tuple(e1, e2) => e1
                .variables()
                .into_iter()
                .chain(e2.variables().into_iter())
                .collect(),
            _ => vec![],
        }
    }
}
