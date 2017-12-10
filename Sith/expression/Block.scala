package expression

import value._

case class Block(val locals: List[Expression]) extends SpecialForm {
  def execute(env: Environment) = {
  // 1. create a localEnv
  // 2. local.execute(localEnv)
  // 3. return last local
    val localEnv = new Environment
    val values = locals.map(x => x.execute(env))
    values.last
  }
}