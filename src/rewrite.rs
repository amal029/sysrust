use sysrust::ast::*;

pub struct State {
    labelnum: u64,
}

impl State {
    pub fn new() -> Self {
        State { labelnum: 0 }
    }
    fn get_label_num(&mut self) -> u64 {
        let r = self.labelnum;
        self.labelnum += 1;
        return r;
    }
}

pub fn rewrite_stmts(stmts: Vec<Stmt>, state: &mut State) -> Vec<Stmt> {
    stmts
        .into_iter()
        .map(|x| imm_rewrite(x, state))
        .collect::<Vec<Stmt>>()
}

// XXX: Consume the ast and give back a new ast with weak and immediate
// aborts rewritten.
fn imm_rewrite(_ast: Stmt, state: &mut State) -> Stmt {
    fn rewrite_option(s: Option<Box<Stmt>>, state: &mut State) -> Option<Box<Stmt>> {
        match s {
            Some(x) => Some(Box::new(imm_rewrite(*x, state))),
            None => None,
        }
    }
    fn abort_rewrite(a: Option<ASQual>, s: Stmt, sy: Symbol, p: (usize, usize)) -> Stmt {
        match a {
            Some(x) => match x {
                ASQual::Immediate => Stmt::Present(
                    Expr::Esymbol(sy.clone(), p),
                    Box::new(Stmt::Noop(p)),
                    Some(Box::new(Stmt::Abort(sy, None, Box::new(s), p))),
                    p,
                ),
                ASQual::Weak => Stmt::Abort(sy, Some(ASQual::Weak), Box::new(s), p),
                ASQual::WeakImmediate => todo!("Weak and immediate abort"),
            },
            None => Stmt::Abort(sy, a, Box::new(s), p),
        }
    }
    fn suspend_rewrite(
        a: Option<ASQual>,
        s: Stmt,
        sy: Symbol,
        p: (usize, usize),
        state: &mut State,
    ) -> Stmt {
        match a {
            Some(x) => match x {
                ASQual::Immediate => {
                    let mut ss = String::from("S");
                    ss.push_str(state.get_label_num().to_string().as_str());
                    let pa = Stmt::Pause(Symbol::Symbol(ss, p), p);
                    let lop = Stmt::Loop(Box::new(pa), p);
                    let ab = Stmt::Abort(sy.clone(), None, Box::new(lop), p);
                    let sus = Stmt::Suspend(sy, None, Box::new(s), p);
                    Stmt::Block(vec![ab, sus], p)
                }
                ASQual::Weak => Stmt::Abort(sy, Some(ASQual::Weak), Box::new(s), p),
                ASQual::WeakImmediate => todo!("Weak and immediate suspend"),
            },
            None => Stmt::Abort(sy, a, Box::new(s), p),
        }
    }
    match _ast {
        Stmt::Block(x, p) => Stmt::Block(rewrite_stmts(x, state), p),
        Stmt::Pause(s, p) => Stmt::Pause(s, p),
        Stmt::Emit(s, v, p) => Stmt::Emit(s, v, p),
        Stmt::Present(e, s, o, p) => Stmt::Present(
            e,
            Box::new(imm_rewrite(*s, state)),
            rewrite_option(o, state),
            p,
        ),
        Stmt::Signal(s, o, p) => Stmt::Signal(s, o, p),
        Stmt::Abort(s, o, st, p) => {
            let sg = imm_rewrite(*st, state);
            abort_rewrite(o, sg, s, p)
        }
        Stmt::Suspend(s, o, st, p) => {
            let sg = imm_rewrite(*st, state);
            suspend_rewrite(o, sg, s, p, state)
        }
        Stmt::Loop(s, p) => Stmt::Loop(Box::new(imm_rewrite(*s, state)), p),
        Stmt::Assign(s, e, p) => Stmt::Assign(s, e, p),
        Stmt::Noop(p) => Stmt::Noop(p),
        Stmt::Spar(sts, p) => Stmt::Spar(rewrite_stmts(sts, state), p),
    }
}

// XXX: Write the function to rewrite statements to the fsm graph

#[derive(Debug, Clone)]
struct GraphNode {
    children: Vec<usize>, // this is the destination state
    parents: Vec<usize>,  //this is the parent state
    actions: Vec<Stmt>,   // these are the actions on the transitions
    guards: Vec<Expr>,    // these are the guards on transitions
    tag: bool,            // this is to tell if this is a real state of
    // dummy state
    label: String,
    idx: Index,
}

impl Default for GraphNode {
    fn default() -> Self {
        Self {
            children: vec![],
            parents: vec![],
            actions: vec![],
            guards: vec![],
            tag: false,
            label: String::from("Initial"),
            idx: 0,
        }
    }
}

type Index = usize;

fn rewrite_stmt_to_graph_fsm(
    _nodes: &mut Vec<GraphNode>,
    idx: &mut usize,
    s: &Stmt,
) -> (Vec<Index>, Vec<Index>) {
    fn symbol_string(s: &Symbol) -> String {
        match s {
            Symbol::Symbol(ss, _) => ss.clone(),
        }
    }
    match s {
        Stmt::Emit(a, expr, pos) => {
            let mut i = GraphNode::default();
            i.label = String::from("EmitStart");
            i.actions
                .push(Stmt::Emit(a.clone(), expr.clone(), pos.clone()));
            i.guards.push(Expr::True(pos.clone()));
            let mut e = GraphNode::default();
            e.label = String::from("EmitEnd");
            // XXX: Put these in the _nodes
            _nodes.push(i);
            _nodes[*idx].children.push(*idx + 1);
            _nodes[*idx].idx = *idx;
            let r1 = vec![*idx];
            *idx += 1;
            _nodes.push(e);
            _nodes[*idx].parents.push(*idx - 1);
            _nodes[*idx].idx = *idx;
            let r2 = vec![*idx];
            *idx += 1;
            (r1, r2)
        }
        Stmt::Pause(la, pos) => {
            let mut s0 = GraphNode::default();
            s0.label = String::from(symbol_string(&la));
            s0.tag = true;
            s0.guards.push(Expr::True(pos.clone()));
            let mut e = GraphNode::default();
            e.label = String::from("PauseEnd");
            // XXX: Add the doubly linked list annotations
            _nodes.push(s0);
            _nodes[*idx].children.push(*idx + 1);
            _nodes[*idx].idx = *idx;
            let r1 = vec![*idx];
            *idx += 1;
            _nodes.push(e);
            _nodes[*idx].parents.push(*idx - 1);
            _nodes[*idx].idx = *idx;
            let r2 = vec![*idx];
            *idx += 1;
            (r1, r2)
        }
        Stmt::Assign(a, expr, pos) => {
            let mut i = GraphNode::default();
            i.actions
                .push(Stmt::Assign(a.clone(), expr.clone(), pos.clone()));
            i.guards.push(Expr::True(pos.clone()));
            let mut e = GraphNode::default();
            e.label = String::from("End");
            // XXX: Add the doubly linked list annotations
            _nodes.push(i);
            _nodes[*idx].children.push(*idx + 1);
            _nodes[*idx].idx = *idx;
            let r1 = vec![*idx];
            *idx += 1;
            _nodes.push(e);
            _nodes[*idx].parents.push(*idx - 1);
            _nodes[*idx].idx = *idx;
            let r2 = vec![*idx];
            *idx += 1;
            (r1, r2)
        }
        Stmt::Signal(a, io, pos) => {
            let mut i = GraphNode::default();
            i.label = String::from("SignalStart");
            i.actions
                .push(Stmt::Signal(a.clone(), io.clone(), pos.clone()));
            i.guards.push(Expr::True(pos.clone()));
            let mut e = GraphNode::default();
            e.label = String::from("SignalEnd");
            // XXX: Add the doubly linked list annotations
            _nodes.push(i);
            _nodes[*idx].children.push(*idx + 1);
            _nodes[*idx].idx = *idx;
            let r1 = vec![*idx];
            *idx += 1;
            _nodes.push(e);
            _nodes[*idx].parents.push(*idx - 1);
            _nodes[*idx].idx = *idx;
            let r2 = vec![*idx];
            *idx += 1;
            (r1, r2)
        }
        Stmt::Noop(pos) => {
            let mut i = GraphNode::default();
            i.label = String::from("NoopStart");
            i.guards.push(Expr::True(pos.clone()));
            let mut e = GraphNode::default();
            e.label = String::from("NoopEnd");
            // XXX: Put these in the _nodes
            _nodes.push(i);
            _nodes[*idx].children.push(*idx + 1);
            _nodes[*idx].idx = *idx;
            let r1 = vec![*idx];
            *idx += 1;
            _nodes.push(e);
            _nodes[*idx].parents.push(*idx - 1);
            _nodes[*idx].idx = *idx;
            let r2 = vec![*idx];
            *idx += 1;
            (r1, r2)
        }
        Stmt::Block(_stmts, _pos) => rewrite_to_graph_fsm(&_stmts, idx, _nodes),
	// XXX: With no else branch
        Stmt::Present(_expr, _tb, None, _pos) => {
            // XXX: First make the body
            let (_tr1, _tr2) = rewrite_stmt_to_graph_fsm(_nodes, idx, _tb);
            let (_er1, _er2) = rewrite_stmt_to_graph_fsm(_nodes, idx, &Stmt::Noop(_pos.clone()));

            // XXX: Now make the initial node for the if-else statement
            let ii = *idx;
            let mut i = GraphNode::default();
            i.idx = ii;
            i.label = String::from("PresentStart");
            // XXX: Add the edges between i and nodes in _tr1 and _er1
            _tr1.into_iter().for_each(|x| {
                i.children.push(x);
                _nodes[x].parents.push(ii);
            });
            _er1.into_iter().for_each(|x| {
                i.children.push(x);
                _nodes[x].parents.push(ii);
            });
            // XXX: Push the node to the vector
            _nodes.push(i);
            *idx += 1;

            // XXX: Returning the initial and end indices
            let mut yy = _tr2;
            let mut yy1 = _er2;
            yy.append(&mut yy1);
            (vec![ii], yy)
        }
	// XXX: With an else branch
        Stmt::Present(_expr, _tb, Some(_eb), _pos) => {
            // XXX: First make the body
            let (_tr1, _tr2) = rewrite_stmt_to_graph_fsm(_nodes, idx, _tb);
            let (_er1, _er2) = rewrite_stmt_to_graph_fsm(_nodes, idx, _eb);

            // XXX: Now make the initial node for the if-else statement
            let ii = *idx;
            let mut i = GraphNode::default();
            i.idx = ii;
            i.label = String::from("PresentStart");
            // XXX: Add the edges between i and nodes in _tr1 and _er1
            _tr1.into_iter().for_each(|x| {
                i.children.push(x);
                _nodes[x].parents.push(ii);
            });
            _er1.into_iter().for_each(|x| {
                i.children.push(x);
                _nodes[x].parents.push(ii);
            });
            // XXX: Push the node to the vector
            _nodes.push(i);
            *idx += 1;

            // XXX: Returning the initial and end indices
            let mut yy = _tr2;
            let mut yy1 = _er2;
            yy.append(&mut yy1);
            (vec![ii], yy)
        }
        Stmt::Abort(_a, _at, _body, _pos) => todo!(),
        Stmt::Suspend(_a, _at, _body, _pos) => todo!(),
        Stmt::Loop(_body, _pos) => todo!(),
        Stmt::Spar(_stmts, _pos) => todo!(),
    }
}

#[allow(dead_code)]
fn rewrite_to_graph_fsm(
    _v: &[Stmt],
    _idx: &mut Index,
    _nodes: &mut Vec<GraphNode>,
) -> (Vec<Index>, Vec<Index>) {
    let mut r1 = vec![];
    let mut r2 = vec![];
    let mut _pe: Vec<Index> = vec![];
    for (ii, i) in _v.iter().enumerate() {
        let (si, ei) = rewrite_stmt_to_graph_fsm(_nodes, _idx, i);
        if ii == 0 {
            r1 = si.clone();
        }
        if ii == _v.len() - 1 {
            r2 = ei.clone();
        }
        // XXX: Now you need to merge the nodes one after the other
        if ii > 0 {
            for k in _pe {
                for &l in si.iter() {
                    _nodes[k].children.push(l);
                    _nodes[l].parents.push(k);
                }
            }
        }
        _pe = ei;
    }
    // TODO: Change this later
    (r1, r2)
}
