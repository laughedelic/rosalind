// Here are solutions to problems from Rosalind project (see links)

// Common functions:

def updMap[K,V](m: Map[K,V], key: K, f: V => V): Map[K,V] =
    if(m contains key) m + (key -> f(m(key))) else m

// Problems:

//// Counting Bases: http://rosalind.info/problems/acgu/ ///////////////////////
// Given: An RNA string s of length at most 1000 nt.
// Return: Four integers corresponding to the number of times that the symbols A, C, G, and U occur in s.

def countBases(rna: String): Map[Char, Int] =
	rna.foldLeft(Map('A' -> 0, 'C' -> 0, 'G' -> 0, 'U' -> 0))(
        (acc, x) => updMap(acc, x, (_:Int) + 1) )

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
