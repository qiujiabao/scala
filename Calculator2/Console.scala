
object Console {
  
  def execute(cmmd: String): String = {
    if (cmmd.equals("quit")) "bye"
    else if (cmmd.equals("help")) "commands: add, mul, sub, div, quit, help" + '\n'
    else
    {
      val input = cmmd.split("\\s+")
      if (input(0).equals("add"))
      {
        var except = false
        var result = 0.0
        for (i <- 1 until input.size)
        {
          try result += input(i).toDouble
          catch {case e: Exception => {println("invalid argument: " + input(i)); except = true}}
        }
        if (!except) result.toString() + '\n'
        else ""
      }
      else if (input(0).equals("sub"))
      {
        var except = false
        var result = 0.0
        try result = input(1).toDouble
        catch {case e: Exception => {println("invalid argument: " + input(1)); except = true}}
        for (i <- 2 until input.size)
        {
          try result -= input(i).toDouble
          catch {case e: Exception => {println("invalid argument: " + input(i)); except = true}}
        }
        if (!except) result.toString() + '\n'
        else ""
      }
      else if (input(0).equals("mul"))
      {
        var except = false
        var result = 1.0
        for (i <- 1 until input.size)
        {
          try result *= input(i).toDouble
          catch {case e: Exception => {println("invalid argument: " + input(i)); except = true}}
        }
        if (!except) result.toString() + '\n'
        else ""
        }
      else if (input(0).equals("div"))
      {
        var except = false
        var result = 0.0
        try result = input(1).toDouble
        catch {case e: Exception => {println("invalid argument: " + input(1)); except = true}}
        for (i <- 2 until input.size)
        {
          try result /= input(i).toDouble
          catch {case e: Exception => {println("invalid argument: " + input(i)); except = true}}
        }
        if (!except) result.toString() + '\n'
        else ""
      }
      else "Unrecognized command: " + input(0) + '\n'
    }
  }
  
  def repl {
    var result = ""
    while (!result.equals("bye"))
    {
      val cmmd = readLine("-> ")
      result = execute(cmmd)
      print(result)
    }
  }
  
  def main(args: Array[String]): Unit = { repl }
  
}