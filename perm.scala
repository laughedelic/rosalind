//// Enumerating All Possible Gene Orders: http://rosalind.info/problems/perm/ /
// Given: A positive integer n≤7.
// Return: The total number of permutations of length n, 
// 		   followed by a list of all such permutations (in any order).

def permutations[T](list: List[T]): List[List[T]] = list match {
	case Nil => List(Nil)
	case x :: xs => {
		def insertX(as: List[T]): List[List[T]] = as match {
			case Nil => List(List(x))
			case y :: ys => (x::as) :: (insertX(ys) map (y :: _))
		}
		(permutations(xs) map insertX) flatten
	}
}

def perm(n: Int): String = {
	val ps = permutations(1 to n toList)
	(ps length).toString +"\n"+ (ps map (_.mkString(" "))).mkString("\n")
}
