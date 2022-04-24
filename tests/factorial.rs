use constant_branching_dsl::*;
use std::cmp::Ordering;
use std::rc::Rc;

fn factorial(n: i128) -> Result<i128, &'static str> {
    match n.cmp(&0) {
        Ordering::Less => Err("n must be not be less than 0"),
        Ordering::Equal => Ok(1),
        Ordering::Greater => return Ok(n * factorial(n - 1)?),
    }
}

fn test_factorial(n: i128) {
    let factorial_exp = Exp::Fix(
        None,
        "factorial",
        Rc::new(Exp::Fun(
            None,
            "n",
            Rc::new(Exp::If(
                Rc::new(Exp::BinOp(
                    BinOp::Eq,
                    Rc::new(Exp::Var("n")),
                    Rc::new(Exp::Num(0)),
                )),
                Rc::new(Exp::Num(1)),
                Rc::new(Exp::BinOp(
                    BinOp::Times,
                    Rc::new(Exp::Var("n")),
                    Rc::new(Exp::BinOp(
                        BinOp::Ap,
                        Rc::new(Exp::Var("factorial")),
                        Rc::new(Exp::BinOp(
                            BinOp::Minus,
                            Rc::new(Exp::Var("n")),
                            Rc::new(Exp::Num(1)),
                        )),
                    )),
                )),
            )),
        )),
        34,
        Rc::new(Exp::Num(0)),
    );

    assert_eq!(
        Exp::BinOp(BinOp::Ap, Rc::new(factorial_exp), Rc::new(Exp::Num(n)))
            .evaluate()
            .unwrap(),
        Exp::Num(factorial(n).unwrap())
    );
}

#[test]
fn test_0() {
    test_factorial(0);
}

#[test]
fn test_1() {
    test_factorial(1);
}

#[test]
fn test_2() {
    test_factorial(2);
}

#[test]
fn test_3() {
    test_factorial(3);
}

#[test]
fn test_4() {
    test_factorial(4);
}

#[test]
fn test_33() {
    test_factorial(33);
}
