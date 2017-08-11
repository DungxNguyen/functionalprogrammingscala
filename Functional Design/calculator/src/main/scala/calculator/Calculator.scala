package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
    namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map {
      case (variable, expr) => (variable, Signal(
        if (checkNoLoop(Set(variable), expr(), namedExpressions)) eval(expr(), namedExpressions) else Double.NaN))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v)   => v
      case Plus(a, b)   => eval(a, references) + eval(b, references)
      case Minus(a, b)  => eval(a, references) - eval(b, references)
      case Times(a, b)  => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
      case Ref(name)    => eval(getReferenceExpr(name, references), references)
    }
  }

  def checkNoLoop(name: Set[String], expr: Expr, references: Map[String, Signal[Expr]]): Boolean = {
    def checkBothBrand(name: Set[String], expr: Expr, expr2: Expr, references: Map[String, Signal[Expr]]): Boolean = {
      checkNoLoop(name, expr, references) && checkNoLoop(name, expr2, references)
    }

    expr match {
      case Literal(v)   => true
      case Ref(n)       => (!name.contains(n) && checkNoLoop(name + n, getReferenceExpr(n, references), references))
      case Plus(a, b)   => checkBothBrand(name, a, b, references)
      case Minus(a, b)  => checkBothBrand(name, a, b, references)
      case Times(a, b)  => checkBothBrand(name, a, b, references)
      case Divide(a, b) => checkBothBrand(name, a, b, references)
    }
  }

  /**
   * Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
