object VectorCalculator {
  
  def add(v1: Array[Int], v2: Array[Int]) = {
    // complete this method
    var x = v1(0) + v2(0)
    var y = v1(1) + v2(1)
    var z = v1(2) + v2(2)
    var result = Array(x, y, z)
    result
  }
  
  def dot(v1: Array[Int], v2: Array[Int]) = {
    // complete this method
    var result = v1(0) * v2(0) + v1(1) * v2(1) + v1(2) * v2(2)
    result
  }
  
  def toString(v: Array[Int]) = {
    var result = "["
    for(e <- v) {
      result = result + " " + e
    }
    result = result + "]"
    result
  }

  def main(args: Array[String]): Unit = {
    try {
      print("Enter 3 integers: ")
      var x = readInt()
      var y = readInt()
      var z = readInt()
      val vec1 = Array(x, y, z)
      val vec2 = Array(1, 2, 3)
      val vec3 = add(vec1, vec2)
      println("sum = " + toString(vec3))
      println("dot = " + dot(vec1, vec2))
    } catch {
         case e: Exception => {println(e)}
    }
    
  }

}
