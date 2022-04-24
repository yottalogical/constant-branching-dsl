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
    Fix(Option<Typ>, &'static str, Rc<Exp>, usize),
    Triv,
}

#[derive(PartialEq, Debug)]
pub enum EvaluationErr {
    NegNonNum,
    ApNum,
    FunNonAp,
    InvalidBinOp,
    IfNonBool,
    IfNonNum,
    UnassignedVar,
    FixLimit,
}

impl Exp {
    pub fn evaluate(&self) -> Result<Exp, EvaluationErr> {
        match self {
            Exp::UnOp(UnOp::Neg, e) => {
                if let Ok(Exp::Num(n)) = e.evaluate() {
                    Ok(Exp::Num(-n))
                } else {
                    Err(EvaluationErr::NegNonNum)
                }
            }
            Exp::BinOp(op, e1, e2) => match (e1.evaluate()?, e2.evaluate()?) {
                (Exp::Num(n1), Exp::Num(n2)) => match op {
                    BinOp::Lt => Ok(Exp::Bool(n1 < n2)),
                    BinOp::Gt => Ok(Exp::Bool(n1 > n2)),
                    BinOp::Eq => Ok(Exp::Bool(n1 == n2)),
                    BinOp::Plus => Ok(Exp::Num(n1 + n2)),
                    BinOp::Minus => Ok(Exp::Num(n1 - n2)),
                    BinOp::Times => Ok(Exp::Num(n1 * n2)),
                    BinOp::Ap => Err(EvaluationErr::ApNum),
                },
                (Exp::Fun(_, x, body), e2) => {
                    if let BinOp::Ap = op {
                        body.substitute(&e2, x).evaluate()
                    } else {
                        Err(EvaluationErr::FunNonAp)
                    }
                }
                _ => Err(EvaluationErr::InvalidBinOp),
            },
            Exp::If(e1, e2, e3) => {
                if let Exp::Bool(b) = e1.evaluate()? {
                    if let (Exp::Num(n2), Exp::Num(n3)) = (e2.evaluate()?, e3.evaluate()?) {
                        let mask_for_n2 = -(b as i128);
                        let mask_for_n3 = b as i128 - 1;

                        let masked_n2 = mask_for_n2 & n2;
                        let masked_n3 = mask_for_n3 & n3;

                        Ok(Exp::Num(masked_n2 | masked_n3))
                    } else {
                        Err(EvaluationErr::IfNonBool)
                    }
                } else {
                    Err(EvaluationErr::IfNonNum)
                }
            }
            Exp::Var(_) => Err(EvaluationErr::UnassignedVar),
            Exp::Let(_, e1, x, e2) => e2.substitute(e1, x).evaluate(),
            Exp::Fix(t, x, e1, recurses_left) => {
                if *recurses_left > 0 {
                    let reduced_self = Exp::Fix(t.clone(), x, e1.clone(), recurses_left - 1);
                    e1.substitute(&reduced_self, x).evaluate()
                } else {
                    Err(EvaluationErr::FixLimit)
                }
            }
            Exp::Num(_) | Exp::Bool(_) | Exp::Fun(_, _, _) | Exp::Triv => Ok(self.clone()),
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
            Exp::Fix(t, x1, e1, recurses_left) => {
                if *x1 == x {
                    self.clone()
                } else {
                    Exp::Fix(
                        t.clone(),
                        x1,
                        Rc::new(e1.substitute(sub, x)),
                        *recurses_left,
                    )
                }
            }
            Exp::Num(_) | Exp::Bool(_) | Exp::Triv => self.clone(),
        }
    }
}
