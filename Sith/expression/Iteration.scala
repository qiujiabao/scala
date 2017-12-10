package expression

import value._
import system._

case class Iteration(val condition: Expression, val body: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    //val boo = condition.execute(env)
    //if (!boo.isInstanceOf[Boole]) throw new TypeException
    //val con = boo.asInstanceOf[Boole].value
    //while (con) body.execute(env)
    var con = true
    while(con) {
      val boo = condition.execute(env)
      if (!boo.isInstanceOf[Boole]) throw new TypeException("Condition must be a Boole.")
      con = boo.asInstanceOf[Boole].value
      if (con) body.execute(env)
    }
    Notification.DONE
  }
}