package expressions

/**
 * Error for missing variable bindings in evaluation.
 */
case class UnknownValueException(v: Variable) extends Exception(v.toString + " could not be bound.")

/**
 * An abstract expression node for exercise round 16, exercises 1,2,3,4.
 */
sealed trait Exp {
  def value(bindings: Map[Variable, Double]): Double
}

/**
 * A variable can get a value in evaluation, see `value` method.
 */
case class Variable(name: String) extends Exp {
  def value(bindings: Map[Variable, Double]) = bindings.getOrElse(this, throw new UnknownValueException(this))
}

/**
 * A constant is fixed to a numeric value.
 */
case class Const(c: Double) extends Exp {
  def value(bindings: Map[Variable, Double]) = c
}

/**
 * Addition joins two expression nodes into a sum.
 */
case class Add(u: Exp, v: Exp) extends Exp {
  def value(bindings: Map[Variable, Double]) = u.value(bindings) + v.value(bindings)
}

/**
 * Multiplication joins two expression nodes into a product.
 */
case class Mul(u: Exp, v: Exp) extends Exp {
  def value(bindings: Map[Variable, Double]) = u.value(bindings) * v.value(bindings)
}

/**
 *  Provided as an example only - DO NOT use this in exercises.
 */
object Add {
  def apply(e: Exp*): Exp = e.toList match {
    case Nil => Const(0)
    case f :: Nil => f
    case f :: g => new Add(f, apply(g: _*))
  }
}
