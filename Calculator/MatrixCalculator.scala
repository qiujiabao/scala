import Array._

object MatrixCalculator {
  
  // converts matrix to a string
  def toString(matrix: Array[Array[Int]]) = {
    // place code here
    var result = "\r\n\r\n"
    for (i <- 0 until matrix.length)
    {
      for (j <- 0 until matrix.length - 1)
      {
        result += matrix(i)(j) + " "
      }
      result += matrix(i)(matrix.length - 1) + "\r\n"
    }
    result
  }
  
  // returns the sum of the diagonal entries of a matrix
  def trace(m: Array[Array[Int]]) = {
    // place code here
    var sum = 0;
    for (i <- 0 until m.length)
    {
      sum += m(i)(i)
    }
    sum
  }
  
  // returns a dim x dim matrix with i/j entry = 3 * i + 2 * j % cap
  def makeArray(dim: Int, cap: Int = 100) = {
    // place code here
    var matrix = new Array[Array[Int]](dim)
    for (i <- 0 until dim)
    {
      var row = new Array[Int](dim)
      for (j <- 0 until dim)
      {
        row(j) = 3 * i + 2 * j % cap
      }
      matrix(i) = row
    }
    matrix
  }

  def main(args: Array[String]): Unit = {
    print("Enter a positive integer: ")
    var n = readInt
    var m = makeArray(n)
    println(toString(m))
    println("trace = " + trace(m))
  }

}
