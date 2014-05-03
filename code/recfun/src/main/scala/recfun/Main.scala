package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
  {
    if(0 == c || 0 == r || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1) 
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
  {
    val filterChars = chars.filter((c: Char) => (c == '(' || c == ')'))
    balance_clear(filterChars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
  {
    if(0 == money) 1
    else if(coins.isEmpty) 0
    else if(money < 0) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  } 
  
  def balance_clear(filterChars: List[Char]): Boolean = 
  {
    val length = filterChars.length
    if(0 == length) true
    else if(length % 2 != 0) false
    else if(filterChars.apply(0) == '(' && filterChars.apply(1) == ')') balance_clear(filterChars.slice(2, filterChars.length))
    else if(filterChars.apply(filterChars.length - 2) == '(' && filterChars.apply(filterChars.length - 1) == ')') balance_clear(filterChars.slice(0, filterChars.length - 2))
    else if(filterChars.head == '(' && filterChars.last == ')') balance_clear(filterChars.slice(1, filterChars.length - 1)) 
    else false
  }
}
