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
