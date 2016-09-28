package exercises

object Fibonacci {
  
  /**
   * Implement the classic Fibonacci sequence, where
   *    fib(1) = 1
   *    fib(2) = 1
   *    fib(n) = fib(n-1) + fib(n-2)
   */
  def fib(n: Int): Int = n match {
  	case(1 | 2) => 1
  	case( _ ) => fib(n-1) + fib(n-2)
  }

}