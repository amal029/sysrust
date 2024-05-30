use std::process::exit;

use sysrust::ast::*;

use crate::error::print_bytes;

// XXX: Consume the ast and give back a new ast with weak and immediate
// aborts rewritten.
// XXX: Write the function to rewrite statements to the fsm graph

#[derive(Debug, Clone)]
enum NodeT {
    Spar,
    Seq,
    Branch,
}

#[derive(Debug)]
pub struct GraphNode {
    children: Vec<usize>, // this is the destination state
    parents: Vec<usize>,  // this is the parent state
    actions: Vec<Stmt>,   // these are the actions on the transitions
    guards: Vec<Expr>,    // these are the guards on transitions
    tag: bool,            // this is to tell if this is a real state or dummy
    label: String,
    idx: Index,
    tt: NodeT,
    _tid: Index,
}

impl GraphNode {
    fn default(t: Index) -> Self {
        Self {
            children: vec![],
            parents: vec![],
            actions: vec![],
            guards: vec![],
            tag: false,
            label: String::from("Initial"),
            idx: 0,
            tt: NodeT::Seq,
            _tid: t,
        }
    }
}

type Index = usize;

fn rewrite_stmt_to_graph_fsm(
    ff: &str,
    _nodes: &mut Vec<GraphNode>,
    tid: Index,
    idx: &mut usize,
    s: &Stmt,
) -> (Index, Index) {
    fn symbol_string(s: &Symbol) -> String {
        match s {
            Symbol::Symbol(ss, _) => ss.clone(),
        }
    }
    match s {
        Stmt::Emit(a, expr, pos) => {
            let mut i = GraphNode::default(tid);
            i.label = String::from("EmitStart");
            i.actions
                .push(Stmt::Emit(a.clone(), expr.clone(), *pos));
            i.guards.push(Expr::True(*pos));
            let mut e = GraphNode::default(tid);
            e.label = String::from("EmitEnd");
            // XXX: Put these in the _nodes
            _nodes.push(i);
            _nodes[*idx].children.push(*idx + 1);
            _nodes[*idx].idx = *idx;
            let r1 = _nodes[*idx].idx;
            *idx += 1;
            _nodes.push(e);
            _nodes[*idx].parents.push(*idx - 1);
            _nodes[*idx].idx = *idx;
            let r2 = _nodes[*idx].idx;
            *idx += 1;
            (r1, r2)
        }
        Stmt::Pause(la, pos) => {
            let mut s0 = GraphNode::default(tid);
            s0.label = symbol_string(la);
            s0.tag = true;
            s0.guards.push(Expr::True(*pos));
            let mut e = GraphNode::default(tid);
            e.label = String::from("PauseEnd");
            // XXX: Add the doubly linked list annotations
            _nodes.push(s0);
            _nodes[*idx].children.push(*idx + 1);
            _nodes[*idx].idx = *idx;
            let r1 = _nodes[*idx].idx;
            *idx += 1;
            _nodes.push(e);
            _nodes[*idx].parents.push(*idx - 1);
            _nodes[*idx].idx = *idx;
            let r2 = _nodes[*idx].idx;
            *idx += 1;
            (r1, r2)
        }
        Stmt::Assign(a, expr, pos) => {
            let mut i = GraphNode::default(tid);
            i.actions
                .push(Stmt::Assign(a.clone(), expr.clone(), *pos));
            i.guards.push(Expr::True(*pos));
            let mut e = GraphNode::default(tid);
            e.label = String::from("End");
            // XXX: Add the doubly linked list annotations
            _nodes.push(i);
            _nodes[*idx].children.push(*idx + 1);
            _nodes[*idx].idx = *idx;
            let r1 = _nodes[*idx].idx;
            *idx += 1;
            _nodes.push(e);
            _nodes[*idx].parents.push(*idx - 1);
            _nodes[*idx].idx = *idx;
            let r2 = _nodes[*idx].idx;
            *idx += 1;
            (r1, r2)
        }
        Stmt::Variable(a, vtype, _val, pos) => {
            let mut i = GraphNode::default(tid);
            i.label = String::from("VariableStart");
            i.actions.push(Stmt::Variable(
                a.clone(),
                vtype.clone(),
                _val.clone(),
                *pos,
            ));
            i.guards.push(Expr::True(*pos));
            let mut e = GraphNode::default(tid);
            e.label = String::from("VariableEnd");
            // XXX: Add the doubly linked list annotations
            _nodes.push(i);
            _nodes[*idx].children.push(*idx + 1);
            _nodes[*idx].idx = *idx;
            let r1 = _nodes[*idx].idx;
            *idx += 1;
            _nodes.push(e);
            _nodes[*idx].parents.push(*idx - 1);
            _nodes[*idx].idx = *idx;
            let r2 = _nodes[*idx].idx;
            *idx += 1;
            (r1, r2)
        }
        Stmt::DataSignal(a, io, stype, sval, sop, pos) => {
            let mut i = GraphNode::default(tid);
            i.label = String::from("DataSignalStart");
            i.actions.push(Stmt::DataSignal(
                a.clone(),
                io.clone(),
                stype.clone(),
                sval.clone(),
                sop.clone(),
                *pos,
            ));
            i.guards.push(Expr::True(*pos));
            let mut e = GraphNode::default(tid);
            e.label = String::from("DataSignalEnd");
            // XXX: Add the doubly linked list annotations
            _nodes.push(i);
            _nodes[*idx].children.push(*idx + 1);
            _nodes[*idx].idx = *idx;
            let r1 = _nodes[*idx].idx;
            *idx += 1;
            _nodes.push(e);
            _nodes[*idx].parents.push(*idx - 1);
            _nodes[*idx].idx = *idx;
            let r2 = _nodes[*idx].idx;
            *idx += 1;
            (r1, r2)
        }
        Stmt::Signal(a, io, pos) => {
            let mut i = GraphNode::default(tid);
            i.label = String::from("SignalStart");
            i.actions
                .push(Stmt::Signal(a.clone(), io.clone(), *pos));
            i.guards.push(Expr::True(*pos));
            let mut e = GraphNode::default(tid);
            e.label = String::from("SignalEnd");
            // XXX: Add the doubly linked list annotations
            _nodes.push(i);
            _nodes[*idx].children.push(*idx + 1);
            _nodes[*idx].idx = *idx;
            let r1 = _nodes[*idx].idx;
            *idx += 1;
            _nodes.push(e);
            _nodes[*idx].parents.push(*idx - 1);
            _nodes[*idx].idx = *idx;
            let r2 = _nodes[*idx].idx;
            *idx += 1;
            (r1, r2)
        }
        Stmt::Noop(pos) => {
            let mut i = GraphNode::default(tid);
            i.label = String::from("NoopStart");
            i.guards.push(Expr::True(*pos));
            let mut e = GraphNode::default(tid);
            e.label = String::from("NoopEnd");
            // XXX: Put these in the _nodes
            _nodes.push(i);
            _nodes[*idx].children.push(*idx + 1);
            _nodes[*idx].idx = *idx;
            let r1 = _nodes[*idx].idx;
            *idx += 1;
            _nodes.push(e);
            _nodes[*idx].parents.push(*idx - 1);
            _nodes[*idx].idx = *idx;
            let r2 = _nodes[*idx].idx;
            *idx += 1;
            (r1, r2)
        }
        Stmt::Block(_stmts, _pos) => rewrite_to_graph_fsm(ff, _stmts, tid, idx, _nodes),
        // XXX: With no else branch
        Stmt::Present(_expr, _tb, None, _pos) => {
            // XXX: First make the body
            let (_tr1, _tr2) = rewrite_stmt_to_graph_fsm(ff, _nodes, tid, idx, _tb);
            let (_er1, _er2) =
                rewrite_stmt_to_graph_fsm(ff, _nodes, tid, idx, &Stmt::Noop(*_pos));

            // XXX: Now make the initial node for the if-else statement
            let ii = *idx;
            let mut i = GraphNode::default(tid);
            i.tt = NodeT::Branch;
            i.idx = ii;
            // XXX: Add the then and else branch guards
            i.guards.push(_expr.clone());
            i.guards
                .push(Expr::Not(Box::new(_expr.clone()), *_pos));
            i.label = String::from("PresentStart");

            // XXX: Add the edges between i and nodes in _tr1 and _er1
            i.children.push(_tr1);
            _nodes[_tr1].parents.push(ii);
            i.children.push(_er1);
            _nodes[_er1].parents.push(ii);

            // XXX: Push the node to the vector
            _nodes.push(i);
            let r1 = _nodes[*idx].idx;
            *idx += 1;

            // XXX: Make the end node to connect _tr2 and _er2
            let mut e = GraphNode::default(tid);
            e.idx = *idx;
            e.label = String::from("PresentEnd");
            e.tt = NodeT::Branch;

            // XXX: Add edges between e and _tr2 and _er2
            e.parents.push(_tr2);
            _nodes[_tr2].children.push(e.idx);
            _nodes[_tr2].guards.push(Expr::True(*_pos));
            e.parents.push(_er2);
            _nodes[_er2].children.push(e.idx);
            _nodes[_er2].guards.push(Expr::True(*_pos));

            _nodes.push(e);
            let r2 = _nodes[*idx].idx;
            *idx += 1;

            // XXX: Returning the initial and end indices
            (r1, r2)
        }
        // XXX: With an else branch
        Stmt::Present(_expr, _tb, Some(_eb), _pos) => {
            // XXX: First make the body
            let (_tr1, _tr2) = rewrite_stmt_to_graph_fsm(ff, _nodes, tid, idx, _tb);
            let (_er1, _er2) = rewrite_stmt_to_graph_fsm(ff, _nodes, tid, idx, _eb);

            // XXX: Now make the initial node for the if-else statement
            let ii = *idx;
            let mut i = GraphNode::default(tid);
            i.tt = NodeT::Branch;
            i.idx = ii;
            // XXX: Add the then and else branch guards
            i.guards.push(_expr.clone());
            i.guards
                .push(Expr::Not(Box::new(_expr.clone()), *_pos));
            i.label = String::from("PresentStart");

            // XXX: Add the edges between i and nodes in _tr1 and _er1
            i.children.push(_tr1);
            _nodes[_tr1].parents.push(ii);
            i.children.push(_er1);
            _nodes[_er1].parents.push(ii);

            // XXX: Push the node to the vector
            _nodes.push(i);
            let r1 = _nodes[*idx].idx;
            *idx += 1;

            // XXX: Make the end node to connect _tr2 and _er2
            let mut e = GraphNode::default(tid);
            e.idx = *idx;
            e.label = String::from("PresentEnd");
            e.tt = NodeT::Branch;

            // XXX: Add edges between e and _tr2 and _er2
            e.parents.push(_tr2);
            _nodes[_tr2].children.push(e.idx);
            _nodes[_tr2].guards.push(Expr::True(*_pos));
            e.parents.push(_er2);
            _nodes[_er2].children.push(e.idx);
            _nodes[_er2].guards.push(Expr::True(*_pos));

            _nodes.push(e);
            let r2 = _nodes[*idx].idx;
            *idx += 1;

            // XXX: Returning the initial and end indices
            (r1, r2)
        }
        Stmt::Loop(_body, _pos) => {
            let (_bi, _be) = rewrite_stmt_to_graph_fsm(ff, _nodes, tid, idx, _body);
            // XXX: Attach an edge from end node to the initial node
            _nodes[_be].children.push(_bi);
            _nodes[_be].guards.push(Expr::True(*_pos));
            _nodes[_bi].parents.push(_be);
            // XXX: Perform loop causality analysis
            let mut vis: Vec<bool> = vec![false; _nodes.len()];
            if !loop_causality_analysis(_nodes, &mut vis, _bi, _bi) {
                let _ = print_bytes(ff, _pos.0, _pos.1);
                println!("is not causal");
                exit(1);
            }
            // XXX: Return the initial and end node index
            (_bi, _be)
        }
        Stmt::Abort(_a, None, _body, _pos) => {
            // XXX: This is strong abort
            let (_bi, _be) = rewrite_stmt_to_graph_fsm(ff, _nodes, tid, idx, _body);
            let mut vis = vec![false; _nodes.len()];
            let _aexpr = Expr::Not(Box::new(_a.clone()), *_pos);
            attach_abort_expr(_nodes, _bi, _be, &mut vis, &_aexpr, *_pos);

            // XXX: Now make the end node
            let mut e = GraphNode::default(tid);
            e.idx = *idx;
            e.label = String::from("AbortEnd");
            let r2 = e.idx;
            _nodes.push(e);
            *idx += 1;

            // XXX: Always immediate type abort
            // _nodes[_bi].children.push(r2);
            // _nodes[_bi].guards.push(_a.clone());
            // _nodes[r2].parents.push(_bi);

            // XXX: Now add an edge from every real node in body to "e"
            vis = vec![false; _nodes.len()];
            attach_abort_end(_nodes, _bi, _be, r2, &mut vis, _a);

            // XXX: Now attach the last case -- iff the last does not
            // have children already. If it has children it would be a
            // loop!
            // if _nodes[_be].children.len() == 0 {
            if _nodes[_be].children.is_empty()  {
                _nodes[_be].children.push(r2);
                _nodes[_be].guards.push(Expr::True(*_pos));
                _nodes[r2].parents.push(_be);
            }

            // XXX: Return the initial and end indices
            (_bi, r2)
        }
        Stmt::Abort(_a, Some(ASQual::Immediate), _body, _pos) => {
            // XXX: This is strong immediate abort
            let (_bi, _be) = rewrite_stmt_to_graph_fsm(ff, _nodes, tid, idx, _body);
            let mut vis = vec![false; _nodes.len()];
            let _aexpr = Expr::Not(Box::new(_a.clone()), *_pos);
            attach_abort_expr(_nodes, _bi, _be, &mut vis, &_aexpr, *_pos);

            // XXX: Attach expr to initial node too, because immediate
            if _nodes[_bi].guards.is_empty() {
                _nodes[_bi].guards.push(_aexpr.clone());
            } else {
                for _g in _nodes[_bi].guards.iter_mut() {
                    *_g = Expr::And(Box::new(_aexpr.clone()), Box::new(_g.clone()), *_pos);
                }
            }

            // XXX: Now make the end node
            let mut e = GraphNode::default(tid);
            e.idx = *idx;
            e.label = String::from("AbortEnd");
            let r2 = e.idx;
            _nodes.push(e);
            *idx += 1;

            // XXX: Immediate type abort
            _nodes[_bi].children.push(r2);
            _nodes[_bi].guards.push(_a.clone());
            _nodes[r2].parents.push(_bi);

            // XXX: Now add an edge from every real node in body to "e"
            vis = vec![false; _nodes.len()];
            attach_abort_end(_nodes, _bi, _be, r2, &mut vis, _a);

            // XXX: Now attach the last case -- iff the last does not
            // have children already. If it has children it would be a
            // loop!
            if _nodes[_be].children.is_empty() {
                _nodes[_be].children.push(r2);
                _nodes[_be].guards.push(Expr::True(*_pos));
                _nodes[r2].parents.push(_be);
            }

            // XXX: Return the initial and end indices
            (_bi, r2)
        }
        Stmt::Abort(_a, Some(ASQual::Weak), _body, _pos) => {
            let _ = print_bytes(ff, _pos.0, _pos.1);
            todo!("Not yet supported");
        }
        Stmt::Suspend(_a, None, _body, _pos) => {
            // XXX: This is strong suspend
            let (_bi, _be) = rewrite_stmt_to_graph_fsm(ff, _nodes, tid, idx, _body);
            let _aexpr = Expr::Not(Box::new(_a.clone()), *_pos);

            // XXX: Get the guards for every real node
            let mut vis = vec![false; _nodes.len()];
            add_suspend_expr(_nodes, &mut vis, _bi, &_aexpr, _be, *_pos);

            let _aexpr = Expr::Not(Box::new(_a.clone()), *_pos);

            (_bi, _be)
        }
        Stmt::Suspend(_a, Some(_), _body, _pos) => {
            let _ = print_bytes(ff, _pos.0, _pos.1);
            println!("Cannot happen!");
            exit(1);
        }
        Stmt::Spar(_stmts, _pos) => {
            // XXX: Note that we can get edges outside the current
            // thread. This can be identified with tid. If we go outside
            // the current thread that means we just produce a Done
            // state for that thread. Moreover, the join node will
            // produce a done state if all its incoming threads are in
            // done state. Same for parent threads. If the join node
            // does not produce a done state then next time we loop
            // again and call children threads, that are not yet done.

            // XXX: For each _stmts get the _bi and _ei
            let (_bi, _ei): (Vec<Index>, Vec<Index>) = _stmts
                .iter()
                .enumerate()
                .map(|(ii, x)| rewrite_stmt_to_graph_fsm(ff, _nodes, tid + ii + 1, idx, x))
                .unzip();

            // XXX: Now make the fork node (initial)
            let mut _fi = GraphNode::default(tid);
            _fi.label = String::from("SPARFork");
            _fi.tt = NodeT::Spar;

            // XXX: Attach from _fi to _bis
            _bi.iter().for_each(|&x| {
                _fi.children.push(x);
                _nodes[x].parents.push(*idx);
            });
            _bi.iter().for_each(|_| _fi.guards.push(Expr::True(*_pos)));
            _fi.idx = *idx;
            let r1 = *idx;
            _nodes.push(_fi);
            *idx += 1;

            // XXX: Make the join node (end)
            let mut je = GraphNode::default(tid);
            je.tag = true; //this is a real node
            je.idx = *idx;
            je.label = String::from("SPARJoin");
            je.tt = NodeT::Spar;
            let rm = je.idx;

            // XXX: Attach _eis to je.
            _ei.iter().for_each(|&x| {
                _nodes[x].children.push(je.idx);
                if _nodes[x].children.is_empty() {
                    _nodes[x].guards.push(Expr::True(*_pos));
                } else {
                    _nodes[x].guards.push(Expr::False(*_pos));
                }
                je.parents.push(x);
            });

            // XXX: Add a self-loop -- this is when one thread is done
            // and others are not! -- will be copied inside each thread
            // in the backend.
            je.children.push(je.idx);
            je.guards.push(Expr::True(*_pos));
            je.parents.push(je.idx);

            _nodes.push(je);
            *idx += 1;

            // XXX: Now make the final end node -- get out into the
            // outside thread if all threads in parallel are done.
            let mut e = GraphNode::default(tid);
            e.label = String::from("JoinEnd");
            e.idx = *idx;
            _nodes[rm].children.push(e.idx);
            _nodes[rm].guards.push(Expr::True(*_pos));
            e.parents.push(rm);
            let r2 = e.idx;
            _nodes.push(e);
            *idx += 1;

            (r1, r2)
        }
    }
}

fn add_suspend_expr(
    _nodes: &mut [GraphNode],
    _vis: &mut [bool],
    _s: Index,
    _expr: &Expr,
    _d: Index,
    _pos: (Index, Index),
) {
    assert!(_nodes[_s].children.len() == _nodes[_s].guards.len());
    if !_vis[_s] {
        _vis[_s] = true;
    }
    if _nodes[_s].tag && _s != _d {
        for _g in _nodes[_s].guards.iter_mut() {
            *_g = Expr::And(Box::new(_expr.clone()), Box::new(_g.clone()), _pos);
        }
        // XXX: Add a self loop for the Not of _expr
        _nodes[_s].children.push(_s);
        _nodes[_s].parents.push(_s);
        _nodes[_s]
            .guards
            .push(Expr::Not(Box::new(_expr.clone()), _pos));
    }
    let _childs = _nodes[_s].children.clone();
    for i in _childs {
        if !_vis[i] {
            add_suspend_expr(_nodes, _vis, i, _expr, _d, _pos);
        }
    }
}

fn attach_abort_end(
    _nodes: &mut [GraphNode],
    _s: Index,
    _d: Index,
    _e: Index,
    _vis: &mut [bool],
    _expr: &Expr,
) {
    assert!(_nodes[_s].children.len() == _nodes[_s].guards.len());
    if !_vis[_s] {
        _vis[_s] = true;
    }
    // XXX: Only for a real node
    if _nodes[_s].tag && _s != _d {
        // XXX: Attach the edge to the end node
        _nodes[_s].children.push(_e);
        _nodes[_s].guards.push(_expr.clone());
        _nodes[_e].parents.push(_s);
    }
    let _childs = _nodes[_s].children.clone();
    for i in _childs {
        if !_vis[i] {
            attach_abort_end(_nodes, i, _d, _e, _vis, _expr);
        }
    }
}

fn attach_abort_expr(
    _nodes: &mut [GraphNode],
    _s: Index,
    _d: Index,
    _vis: &mut [bool],
    _expr: &Expr,
    _pos: (usize, usize),
) {
    assert!(_nodes[_s].children.len() == _nodes[_s].guards.len());
    if !_vis[_s] {
        _vis[_s] = true;
    }
    // XXX: Now update the guard for this node
    if _nodes[_s].guards.is_empty() && _s != _d && _nodes[_s].tag {
        _nodes[_s].guards.push(_expr.clone());
    } else if _s != _d && _nodes[_s].tag {
        for _g in _nodes[_s].guards.iter_mut() {
            *_g = Expr::And(Box::new(_expr.clone()), Box::new(_g.clone()), _pos);
        }
    }
    // XXX: Now do the same for all the children
    let _childs = _nodes[_s].children.clone(); // this copy is problematic rust issue
    for i in _childs {
        if !_vis[i] {
            attach_abort_expr(_nodes, i, _d, _vis, _expr, _pos);
        }
    }
}

pub fn rewrite_to_graph_fsm(
    ff: &str,
    _v: &[Stmt],
    _tid: Index,
    _idx: &mut Index,
    _nodes: &mut Vec<GraphNode>,
) -> (Index, Index) {
    let mut r1 = 0usize;
    let mut r2 = 0usize;
    let mut _pe = 0usize;
    for (ii, i) in _v.iter().enumerate() {
        let (si, ei) = rewrite_stmt_to_graph_fsm(ff, _nodes, _tid, _idx, i);
        if ii == 0 {
            r1 = si;
        }
        if ii == _v.len() - 1 {
            r2 = ei;
        }
        // XXX: Now you need to merge the nodes one after the other
        if ii > 0 {
            _nodes[_pe].children.push(si);
            _nodes[_pe].guards.push(Expr::True((0, 0)));
            _nodes[si].parents.push(_pe);
        }
        _pe = ei;
    }
    (r1, r2)
}

// XXX: Loop causality analysis
fn loop_causality_analysis(_nodes: &[GraphNode], vis: &mut [bool], _s: Index, d: Index) -> bool {
    let mut toret = true;
    // XXX: Now check if this is a real node
    if _nodes[_s].tag {
        return true;
    }
    if !vis[_s] {
        vis[_s] = true;
    }
    for &i in _nodes[_s].children.iter() {
        if !vis[i] {
            toret = loop_causality_analysis(_nodes, vis, i, d);
            if !toret {
                break;
            }
        } else if vis[i] && i == d {
            toret = false;
            break;
        }
    }
    toret
}
