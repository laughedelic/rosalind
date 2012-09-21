//// Substrings: http://rosalind.info/problems/subs/ ///////////////////////////
// Given: Two DNA strings s and t (each of length at most 1 kbp).
// Return: All locations of t as a substring of s.

// naive implementation
def findSubstrings(s: String, t:String): List[Int] = {
	if(s.length < t.length) Nil
	else {
		val rest = findSubstrings(s.tail, t).map(_ + 1)
		if(t == s.take(t.length)) 1 :: rest else rest
	}
}

def subs(s: String, t:String): String = findSubstrings(s, t).mkString(" ")

// 
