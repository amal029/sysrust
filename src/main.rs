use std::collections::HashMap;
use std::env;
use sysrust::parse;

use crate::analyse::_analyse_var_signal_uses;
use crate::rewrite::{rewrite_to_graph_fsm, GraphNode};
mod analyse;
mod error;
mod rewrite;
use sysrust::ast::Symbol;

fn main() {
    let args: Vec<String> = env::args().collect();
    let _ast = parse(&args[1]);

    // XXX: Analyse signa/var declaration and their uses
    let mut stack: Vec<HashMap<Symbol, analyse::Type>> = Vec::with_capacity(1000);
    stack.push(HashMap::with_capacity(1000)); // pushed the first hashmap
    stack = _analyse_var_signal_uses(&args[1], &_ast, stack);
    stack.pop(); // removed the final hashmap

    // XXX: Make the FSM graph
    let mut _nodes: Vec<GraphNode> = Vec::with_capacity(1000);
    let mut idx = 0usize;
    let tid = 0usize;
    let (_i, _e) = rewrite_to_graph_fsm(&args[1], &_ast, tid, &mut idx, &mut _nodes);
    dbg!("{:?} {:?} {:?}", _nodes, _i, _e);
}
