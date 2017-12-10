object MathLab {

  // Problem 1
  def solve(a: Double, b: Double, c: Double): Option[(Double, Double)] = {
    val tester = b * b - 4 * a * c
    if (tester < 0 ) None
    else Some((-b+Math.sqrt(tester))/(2 * a), (-b-Math.sqrt(tester))/(2 * a))
  }                                               //> solve: (a: Double, b: Double, c: Double)Option[(Double, Double)]
  
  solve(2, -2, -4)                                //> res0: Option[(Double, Double)] = Some((2.0,-1.0))
  solve(1, 0, 1)                                  //> res1: Option[(Double, Double)] = None
  solve(1, 0, -1)                                 //> res2: Option[(Double, Double)] = Some((1.0,-1.0))
  // +++++++

  // Problem 2
  // dist is a function that takes two double tuples as the input, and gives a double
  def dist(p1: (Double, Double), p2: (Double, Double)) = {
    val (x1, y1) = p1
    val (x2, y2) = p2
    val dist = Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2))
    dist
  }                                               //> dist: (p1: (Double, Double), p2: (Double, Double))Double
  
  dist((1, 1), (0, 0))                            //> res3: Double = 1.4142135623730951
  dist((3, 0), (0, 0))                            //> res4: Double = 3.0
  // +++++++
  
  // Problem 3
  def dot(v1: (Double, Double, Double), v2: (Double, Double, Double)) = {
    val (x1, y1, z1) = v1
    val (x2, y2, z2) = v2
    val result = x1 * x2 + y1 * y2 + z1 * z2
    result
  }                                               //> dot: (v1: (Double, Double, Double), v2: (Double, Double, Double))Double
  
  dot((2.0, 3, 4), (2, 2.0, 2))                   //> res5: Double = 18.0
  // +++++++
  
  // Problem 6
  def isPrime(n: Int) = {
    if (n < 0) throw new Exception("Input is negative")
    var isPrime = true
    if (n < 2) isPrime = false
    else{
      for (i <- 2 until n)
        if (n % i == 0) isPrime = false
    }
    isPrime
  }                                               //> isPrime: (n: Int)Boolean
  
  isPrime(0)                                      //> res6: Boolean = false
  isPrime(1)                                      //> res7: Boolean = false
  isPrime(2)                                      //> res8: Boolean = true
  isPrime(8)                                      //> res9: Boolean = false
  // +++++++
  
  // Problem 7
  def phi(n: Int) = {
    if (n < 0) throw new Exception("Input is negative")
    var result = 0
    def gcd(n: Int, k: Int): Int =
    {
      if (n == k) k
      else if (n > k) gcd(n - k, k)
      else gcd(n, k - n)
    }
    for (i <- 1 to n) if (gcd(n, i) == 1) result += 1
    result
  }                                               //> phi: (n: Int)Int
  
  phi(9)                                          //> res10: Int = 6
  phi(10)                                         //> res11: Int = 4
  // +++++++
  
  // Problem 8
  def rollDice ={
    val random = scala.util.Random
    val result1 = random.nextInt(6) + 1
    val result2 = random.nextInt(6) + 1
    (result1, result2)
  }                                               //> rollDice: => (Int, Int)
  
  rollDice                                        //> res12: (Int, Int) = (3,3)
  rollDice                                        //> res13: (Int, Int) = (2,2)
  rollDice                                        //> res14: (Int, Int) = (5,5)
  // +++++++
}