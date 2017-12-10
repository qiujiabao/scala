object StringLab {
  
  // Problem 1
  def isPal(s: String) = {
    var i = 0
    var k = s.size - 1
    var pal = true
    while (i <= k)
    {
      if (s(i) != s(k)) pal = false
      i += 1
      k -= 1
    }
    pal
  }                                               //> isPal: (s: String)Boolean
  
  isPal("rotator")                                //> res0: Boolean = true
  isPal("cat")                                    //> res1: Boolean = false
  // +++++++
  
  // Problem 2
  def isPal2(s: String) = {
    val split = s.split("[\\s+\\,+\\!+\\.+\\?+]")
    val s2 = split.mkString("").toLowerCase()
    isPal(s2)
  }                                               //> isPal2: (s: String)Boolean
  
  isPal2("A man, a plan, a canal, Panama!")       //> res2: Boolean = true
  // +++++++
  
  // Problem 3
  def mkPal(s: String) = {
    def helper(count: Int, output: String): String =
      if (count < 0) output else helper(count - 1, output + s(count))
    helper(s.size - 1, s)
  }                                               //> mkPal: (s: String)String
  
  mkPal("mars")                                   //> res3: String = marssram
  mkPal("3X@#")                                   //> res4: String = 3X@##@X3
  // +++++++
  
  // Problem 4
  def mkWord(n: Int = 10) = {
    var string = ""
    val random = scala.util.Random
    for (i <- 0 to n)
    {
      val ucode = random.nextInt(26) + 97
      string += ucode.toChar
    }
    string
  }                                               //> mkWord: (n: Int)String
  
  mkWord()                                        //> res5: String = iwkwklkkigo
  mkWord()                                        //> res6: String = loybzjpuqon
  mkWord()                                        //> res7: String = ttsxqvfzayg
  mkWord(20)                                      //> res8: String = uwqvoxmgtiwhacmhrnapg
  // +++++++
  
  // Problem 5
  def mkSentence(n: Int = scala.util.Random.nextInt(10)) = {
    val random = scala.util.Random
    var string = ""
    for (i <- 0 to n)
    {
      if (i == 0)
      {
        string = mkWord(random.nextInt(10))
      }
      string += (" " + mkWord(random.nextInt(10)))
    }
    string = string.substring(0, 1).toUpperCase + string.substring(1) + "."
    string
  }                                               //> mkSentence: (n: Int)String
  
  mkSentence()                                    //> res9: String = Ygysklabph zvvomddgo lsen plcp k xcuprckkz.
  mkSentence()                                    //> res10: String = Tlgwmye ncfxi iprqxz ravsiiuval t ulwkrqem uzslkzkr nkzer c
                                                  //| unsuvbxc sdtrhsjsnk.
  mkSentence()                                    //> res11: String = Tumzlekzoi ocxfrzkql vbsttwansi.
  mkSentence(5)                                   //> res12: String = Djcef fejllbn sashc uexaepqda wjws nc sswyu.
  // +++++++
  
}