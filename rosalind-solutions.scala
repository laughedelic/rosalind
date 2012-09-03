// Here are solutions to problems from Rosalind project (see links)

// Common functions:

def updMap[K,V](m: Map[K,V], key: K, f: V => V): Map[K,V] =
    if(m contains key) m + (key -> f(m(key))) else m

// Problems:

//// Counting Bases: http://rosalind.info/problems/acgu/ ///////////////////////
// Given: An RNA string s of length at most 1000 nt.
// Return: Four integers corresponding to the number of times 
//		   that the symbols A, C, G, and U occur in s.

def countBases(rna: String): Map[Char, Int] = {
	val bases = Map('A' -> 0, 'C' -> 0, 'G' -> 0, 'U' -> 0)
	rna.foldLeft(bases)( (acc, x) => updMap(acc, x, (_:Int) + 1) )
}

def acgu(rna: String): String = countBases(rna).values.mkString(" ")
////////////////////////////////////////////////////////////////////////////////


//// Reverse Complement: http://rosalind.info/problems/revc/ ///////////////////
// Given: A DNA string s of length at most 1000 bp.
// Return: The reverse complement of s.

def revc(dna: String): String = 
    dna.reverse.map(b => b match {
        case 'A' => 'T'
        case 'T' => 'A'
        case 'C' => 'G'
        case 'G' => 'C'
    })
////////////////////////////////////////////////////////////////////////////////


//// Substrings: http://rosalind.info/problems/subs/ ///////////////////////////
// Given: Two DNA strings s and t (each of length at most 1 kbp).
// Return: All locations of t as a substring of s.

def findSubstrings(s: String, t:String): List[Int] = {
	if(s.length < t.length) Nil
	else {
		val rest = findSubstrings(s.tail, t).map(_ + 1)
		if(t == s.take(t.length)) 1 :: rest else rest
	}
}

def subs(s: String, t:String): String = findSubstrings(s, t).mkString(" ")
////////////////////////////////////////////////////////////////////////////////


//// RNA Transcription: http://rosalind.info/problems/rna/ /////////////////////
// Given: A DNA string t (of length at most 1 kbp).
// Return: The transcribed RNA string of t (all T's are replaced with U's).

def rna(dna: String): String = 
	dna.map(b => b match {
		case 'T' => 'U'
		case  x  =>  x
	})
////////////////////////////////////////////////////////////////////////////////
