object TaxCalculator {
  
  def tax(income: Double): Double = {
     // enter code here
    if (income < 0) throw new Exception("Income can't be negative")
    else if (income < 20000) 0
    else if (income < 30000) income * 0.05
    else if (income < 40000) income * 0.11
    else if (income < 60000) income * 0.23
    else if (income < 100000) income * 0.32
    else income * 0.5
   } 

}
