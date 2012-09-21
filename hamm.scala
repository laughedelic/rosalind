//// Hamming Distance: http://rosalind.info/problems/hamm/ /////////////////////
// Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).
// Return: The Hamming distance dH(s,t) (number of symbols that differ).

def hamm(s: String, t: String): Int = (s zip t) count ( x => x._1 != x._2 )
