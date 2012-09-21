//// RNA Transcription: http://rosalind.info/problems/rna/ /////////////////////
// Given: A DNA string t (of length at most 1 kbp).
// Return: The transcribed RNA string of t (all T's are replaced with U's).

def rna(dna: String): String = 
	dna.map(b => b match {
		case 'T' => 'U'
		case  x  =>  x
	})
