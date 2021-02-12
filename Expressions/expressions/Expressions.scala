package expressions

import akka.io.Tcp.Bind

/**
 * Playground for testing with expressions. Can be run as Scala application.
 */
object ExpressionTest extends App {
  import Expressions._

  // Exercise 1 in chapter 17.3.

  // Implement the expressions a, b, c and d.

  // Variables used in this exercise:
  def x = Variable("x")
  def y = Variable("y")
  def z = Variable("z")

  // a) 2x

  def func1 = {
    Mul(Const(2), x)
  }

  // b) (2x)^3   eli  2x * 2x * 2x
  // Use the previous result here.

  def func2 = {
    val x2 = func1
    Mul(x2, Mul(x2, x2))
  }

  // c) 3 x y + x (x + 7 z)

  def func3 = {
    val x7z = Add(x, Mul(Const(7), z))
    val xy3 = Mul(Const(3), Mul(x, y))
    Add(xy3, Mul(x, x7z))
  }

  // d) x^2 + 2 x y + y^2

  def func4 = {
    Add(Add(Mul(x, x), Mul(Const(2), Mul(x, y))), Mul(y, y))
  }
  println(prettyprint(Mul(Mul(x, x), x)))

}

/**
 * Implementations for expression handling.
 */
object Expressions {

  // Exercise 2 in chapter 17.3.

  def prettyprint(e: Exp): String = {
    e match {
      case x: Const            => x.c.toString
      case v: Variable         => v.name
      case Add(e: Exp, f: Exp) => "( " + prettyprint(e) + " + " + prettyprint(f) + " )"
      case Mul(e: Exp, f: Exp) if (e == f) => prettyprint(e) + "^2"
      case Mul(e: Exp, f: Exp) if (prettyprint(e) == prettyprint(f) + "^2") => prettyprint(f) + "^3"
      case Mul(e: Exp, f: Exp) if (prettyprint(f) == prettyprint(e) + "^2") => prettyprint(e) + "^3"
      case Mul(e: Exp, f: Exp) => "( " + prettyprint(e) + " * " + prettyprint(f) + " )"
    }
  }

  // Exercise 3 in chapter 17.3.

  def bind(e: Exp, v: Variable, a: Double): Exp = {
    e match {
      case c: Const                  => c
      case matchVar: Variable        => if (matchVar == v) Const(a) else matchVar
      case add @ Add(e: Exp, f: Exp) => {
        val bounde = bind(e, v, a)
        val boundf = bind(f, v, a)
        if (bounde == e && boundf == f) add else Add(bounde, boundf)
      }
      case mul @ Mul(e: Exp, f: Exp) => {
        val bounde = bind(e, v, a)
        val boundf = bind(f, v, a)
        if (bounde == e && boundf == f) mul else Mul(bounde, boundf)
      }
    }
  }

  // Exercise 4 in chapter 17.3.

  def derivate(e: Exp, d: Variable): Exp = {
    e match {
      case Const(_)            => Const(0)
      case v: Variable if (v == d) => Const(1)
      case v: Variable         => Const(0)
      case Add(e: Exp, f: Exp) => Add(derivate(e, d), derivate(f, d))
      case Mul(e: Exp, f: Exp) => Add(Mul(e, derivate(f, d)), Mul(f, derivate(e, d)))
    }
  }

}