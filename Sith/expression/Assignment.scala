package expression

import value._
import system._

case class Assignment(val vbl: Identifier, val update: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    val exe = vbl.execute(env)
    if (!exe.isInstanceOf[Variable]) throw new TypeException("Values can't be updated.")
    val variable = exe.asInstanceOf[Variable]
    variable.content = update.execute(env)
    Notification.DONE
  }
}