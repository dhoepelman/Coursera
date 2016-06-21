package recfun

import scala.annotation.tailrec

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
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, _) | (`r`, _) => 1
    case _ if c <= 0 || r <= 0 || c > r => throw new IllegalArgumentException("Outside of triangle")
    case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def _balance(chars: List[Char], opens: Int = 0): Boolean = chars match {
      case _ if opens < 0 => false
      case Nil => opens == 0
      case '(' :: tail => _balance(tail, opens + 1)
      case ')' :: tail => _balance(tail, opens - 1)
      case _ :: tail => _balance(tail, opens)
    }
    _balance(chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = coins match {
    case _ if money == 0 => 1
    case _ if money < 0 => 0
    case Nil => 0
    case c :: left => countChange(money - c, coins) + countChange(money, left)
  }
}
