object Calculator {

   // = 1 * 2 * 3 * ... * n
   def fact(n: Integer) = {
      // place iterative solution here
     var result = 1
     for (i <- 1 to n)
     {
       result *= i
     }
     result
    }
   
   // = 1 + 2 + 3 + ... + n
   def tri(n: Integer) = {
      // place iterative solution here
     var result = 0
     for (i <- 1 to n)
     {
       result += i
     }
     result
    }
   
   // = 2^n
   def exp(n: Integer) = {
       // place iterative solution here
     var result = 1
     for (i <- 1 to n)
     {
       result *= 2
     }
     result
    }
   
   // = true if n >= 2 and has no smaller divisors
   def isPrime(n: Integer) = {
      // place iterative solution here
     var prime = true;
     if (n < 2) prime = false
     else
     {
       for (i <- 2 until n)
       {
         if (n % i == 0) prime = false;
       }
     }
     prime
   }
   
   def main(args: Array[String]): Unit = {
     println("enter 3 integers x, y, and z on separate lines: ")
     var x = readInt()
     var y = readInt()
     var z = readInt()
     println("fact(x) = " + fact(x))
     println("fact(y) = " + fact(y))
     println("fact(z) = " + fact(z))
     println("tri(x) = " + tri(x))
     println("tri(y) = " + tri(y))
     println("tri(z) = " + tri(z))
     println("exp(x) = " + exp(x))
     println("exp(y) = " + exp(y))
     println("exp(z) = " + exp(z))
     println("isPrime(x) = " + isPrime(x))
     println("isPrime(y) = " + isPrime(y))
     println("isPrime(z) = " + isPrime(z))
   }

}
