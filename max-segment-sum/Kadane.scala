object Kadane extends App {

	def solution(a : Array[Int]) = {
		import java.lang.Math._
		import scala.annotation.tailrec
		
		@tailrec
		def recursion(currentMax : Int, currentSum : Int, index : Int) : Int = 
			if (index == a.length) currentMax
			else {
				val augmentedSum  = currentSum + a(index)
				recursion(max(currentMax, augmentedSum), max(augmentedSum, 0), index+1)
			}
		
		if (a exists (_ > 0))
			recursion(0, 0, 0)
		else
			a.max
	}
	
	// test
	println(solution(Array(1, 2, -3, 5)))
	println(solution(Array(-3, -1, -2, -5)))
}