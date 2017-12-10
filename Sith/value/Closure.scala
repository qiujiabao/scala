package value

import expression._
import system._

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
  def apply(args: List[Value]): Value = {
  // 1. create local env extending defenv  // for static scope rule
  // 2. bind parameters to args in local env
  // 3. body.execute(localenv)
    val localEnv = new Environment(defEnv)
    if (args.length != params.length) throw new TypeException
    for (i <- 0 until params.length) {
      localEnv.put(params(i), args(i))
    }
    body.execute(localEnv)
  }
}