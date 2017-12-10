package expression

import value._
import system._

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
  def execute(env: Environment): Value = {
    val args = operands.map(x => x.execute(env))
    try {
      val fun = operator.execute(env)
      if (!fun.isInstanceOf[Closure]) throw new TypeException
      val clo = fun.asInstanceOf[Closure]
      clo.apply(args)
    }
    catch
    { case ex: UndefinedException => alu.execute(operator, args) }
  }
}