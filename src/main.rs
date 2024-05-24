use std::env;
use sysrust::parse;

use crate::rewrite::{rewrite_to_graph_fsm, GraphNode};
mod error;
mod rewrite;

fn main() {
    let args: Vec<String> = env::args().collect();
    let _ast = parse(&args[1]);
    // let mut _state = rewrite::State::new();
    // XXX: Now rewrite the immediate abort and suspend statement
    // let _ast = rewrite_stmts(_ast, &mut _state);
    // println!("{:?}", _ast);
    // XXX: Make the FSM graph
    let mut _nodes: Vec<GraphNode> = Vec::with_capacity(1000);
    let mut idx = 0usize;
    let tid = 0usize;
    let (_i, _e) = rewrite_to_graph_fsm(&args[1], &_ast, tid, &mut idx, &mut _nodes);
    dbg!("{:?} {:?} {:?}", _nodes, _i, _e);
}
