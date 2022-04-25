use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum Typ {
    Num,
    Bool,
    Arrow(Rc<Typ>, Rc<Typ>),
    Unit,
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnOp {
    Neg,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BinOp {
    Lt,
    Gt,
    Eq,
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    Ap,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Exp {
    Num(i128),
    Bool(bool),
    UnOp(UnOp, Rc<Exp>),
    BinOp(BinOp, Rc<Exp>, Rc<Exp>),
    If(Rc<Exp>, Rc<Exp>, Rc<Exp>),
    Var(&'static str),
    Let(Option<Typ>, Rc<Exp>, &'static str, Rc<Exp>),
    Fun(Option<Typ>, &'static str, Rc<Exp>),
    Fix(Option<Typ>, &'static str, Rc<Exp>, usize, Rc<Exp>),
    Triv,
    Err,
}

impl Exp {
    pub fn evaluate(&self) -> Result<Exp, &'static str> {
        let (result, valid) = self.private_evaluate();

        if valid {
            Ok(result)
        } else {
            Err("Evaluation error")
        }
    }

    fn private_evaluate(&self) -> (Exp, bool) {
        match self {
            Exp::UnOp(UnOp::Neg, e) => {
                if let (Exp::Num(n), valid) = e.private_evaluate() {
                    (Exp::Num(-n), valid)
                } else {
                    (Exp::Num(0), false)
                }
            }
            Exp::BinOp(op, e1, e2) => match (e1.private_evaluate(), e2.private_evaluate()) {
                ((Exp::Num(n1), valid1), (Exp::Num(n2), valid2)) => {
                    let valid = valid1 && valid2;
                    match op {
                        BinOp::Lt => (Exp::Bool(n1 < n2), valid),
                        BinOp::Gt => (Exp::Bool(n1 > n2), valid),
                        BinOp::Eq => (Exp::Bool(n1 == n2), valid),
                        BinOp::Plus => (Exp::Num(n1 + n2), valid),
                        BinOp::Minus => (Exp::Num(n1 - n2), valid),
                        BinOp::Times => (Exp::Num(n1 * n2), valid),
                        BinOp::Div => (Exp::Num(n1 / n2), valid),
                        BinOp::Mod => (Exp::Num(n1 % n2), valid),
                        BinOp::Ap => (Exp::Num(0), false),
                    }
                }
                ((Exp::Fun(_, x, body), valid1), (e2, valid2)) => {
                    if let BinOp::Ap = op {
                        let (result, valid_result) = body.substitute(&e2, x).private_evaluate();
                        (result, valid1 && valid2 && valid_result)
                    } else {
                        (Exp::Err, false)
                    }
                }
                _ => (Exp::Err, false),
            },
            Exp::If(e1, e2, e3) => {
                if let (Exp::Bool(b), valid1) = e1.private_evaluate() {
                    if let ((Exp::Num(n2), valid2), (Exp::Num(n3), valid3)) =
                        (e2.private_evaluate(), e3.private_evaluate())
                    {
                        let mask_for_n2 = -(b as i128);
                        let mask_for_n3 = b as i128 - 1;

                        let masked_n2 = mask_for_n2 & n2;
                        let masked_n3 = mask_for_n3 & n3;

                        let masked_valid2 = !b || valid2;
                        let masked_valid3 = b || valid3;

                        (
                            Exp::Num(masked_n2 | masked_n3),
                            (valid1 && masked_valid2 && masked_valid3),
                        )
                    } else {
                        (Exp::Err, false)
                    }
                } else {
                    (Exp::Err, false)
                }
            }
            Exp::Var(_) => (Exp::Err, false),
            Exp::Let(_, e1, x, e2) => e2.substitute(e1, x).private_evaluate(),
            Exp::Fix(t, x, e1, recurses_left, default) => {
                if *recurses_left > 0 {
                    let reduced_self = Exp::Fix(
                        t.clone(),
                        x,
                        e1.clone(),
                        recurses_left - 1,
                        Rc::clone(default),
                    );
                    e1.substitute(&reduced_self, x).private_evaluate()
                } else {
                    ((**default).clone(), false)
                }
            }
            Exp::Num(_) | Exp::Bool(_) | Exp::Fun(_, _, _) | Exp::Triv => (self.clone(), true),
            Exp::Err => (Exp::Err, false),
        }
    }

    fn substitute(&self, sub: &Exp, x: &str) -> Exp {
        match self {
            Exp::UnOp(op, e1) => Exp::UnOp(op.clone(), Rc::new(e1.substitute(sub, x))),
            Exp::BinOp(op, e1, e2) => Exp::BinOp(
                op.clone(),
                Rc::new(e1.substitute(sub, x)),
                Rc::new(e2.substitute(sub, x)),
            ),
            Exp::If(e1, e2, e3) => Exp::If(
                Rc::new(e1.substitute(sub, x)),
                Rc::new(e2.substitute(sub, x)),
                Rc::new(e3.substitute(sub, x)),
            ),
            Exp::Var(x1) => {
                if *x1 == x {
                    sub.clone()
                } else {
                    self.clone()
                }
            }
            Exp::Let(t, e1, x1, e2) => {
                if *x1 == x {
                    self.clone()
                } else {
                    Exp::Let(
                        t.clone(),
                        Rc::new(e1.substitute(sub, x)),
                        x1,
                        Rc::new(e2.substitute(sub, x)),
                    )
                }
            }
            Exp::Fun(t, x1, e1) => {
                if *x1 == x {
                    self.clone()
                } else {
                    Exp::Fun(t.clone(), x1, Rc::new(e1.substitute(sub, x)))
                }
            }
            Exp::Fix(t, x1, e1, recurses_left, default) => {
                if *x1 == x {
                    self.clone()
                } else {
                    Exp::Fix(
                        t.clone(),
                        x1,
                        Rc::new(e1.substitute(sub, x)),
                        *recurses_left,
                        Rc::clone(default),
                    )
                }
            }
            Exp::Num(_) | Exp::Bool(_) | Exp::Triv | Exp::Err => self.clone(),
        }
    }
}
