// Here are solutions to problems from Rosalind project (see links)

// Common functions:

import scala.io.Source

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


//// Hamming Distance: http://rosalind.info/problems/hamm/ /////////////////////
// Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).
// Return: The Hamming distance dH(s,t) (number of symbols that differ).

def hamm(s: String, t: String): Int = (s zip t) count ( x => x._1 != x._2 )
////////////////////////////////////////////////////////////////////////////////


//// Introduction to Graphs: http://rosalind.info/problems/grph/ ///////////////
// Given: A collection of DNA strings in FASTA format.
// Return: The adjacency list corresponding to the overlap graph for k=3.

def parseGraph(s: String): Map[String, String] =
	s.split('>').tail.foldLeft( Map[String, String]() )( (acc,x) => {
			val y = x.split("\\s+")
			acc + (y.head -> y.tail.mkString) 
		})

def hasOverlap(s1: String, s2: String, k: Int): Boolean =
	(s1 takeRight k) == (s2 take k)

def overlaps(g: Map[String, String], k: Int): List[(String, String)] =
	g.foldLeft( List[(String, String)]() )( (accum, foo) => {
			accum ++ (g.foldLeft( List[(String, String)]() )( (acc, bar) => {
					if(foo._1 != bar._1 && hasOverlap(foo._2, bar._2, k))
						(foo._1, bar._1) :: acc
					else acc
				}))
		})

def grph(s: String, k: Int): String =
	overlaps(parseGraph(s), k).map( x => x._1 +" "+ x._2).mkString("\n")

def grph_(file: String) = println(grph(Source.fromFile(file).mkString, 3))
////////////////////////////////////////////////////////////////////////////////


//// Protein Translation: http://rosalind.info/problems/prot/ //////////////////
// Given: An RNA string s corresponding to a strand of mRNA.
// Return: The protein string encoded by s.

val codonTable = Map(
	 "UUU" -> "F"    ,"CUU" -> "L"    ,"AUU" -> "I"    ,"GUU" -> "V"
	,"UUC" -> "F"    ,"CUC" -> "L"    ,"AUC" -> "I"    ,"GUC" -> "V"
	,"UUA" -> "L"    ,"CUA" -> "L"    ,"AUA" -> "I"    ,"GUA" -> "V"
	,"UUG" -> "L"    ,"CUG" -> "L"    ,"AUG" -> "M"    ,"GUG" -> "V"
	,"UCU" -> "S"    ,"CCU" -> "P"    ,"ACU" -> "T"    ,"GCU" -> "A"
	,"UCC" -> "S"    ,"CCC" -> "P"    ,"ACC" -> "T"    ,"GCC" -> "A"
	,"UCA" -> "S"    ,"CCA" -> "P"    ,"ACA" -> "T"    ,"GCA" -> "A"
	,"UCG" -> "S"    ,"CCG" -> "P"    ,"ACG" -> "T"    ,"GCG" -> "A"
	,"UAU" -> "Y"    ,"CAU" -> "H"    ,"AAU" -> "N"    ,"GAU" -> "D"
	,"UAC" -> "Y"    ,"CAC" -> "H"    ,"AAC" -> "N"    ,"GAC" -> "D"
	,"UAA" -> "Stop" ,"CAA" -> "Q"    ,"AAA" -> "K"    ,"GAA" -> "E"
	,"UAG" -> "Stop" ,"CAG" -> "Q"    ,"AAG" -> "K"    ,"GAG" -> "E"
	,"UGU" -> "C"    ,"CGU" -> "R"    ,"AGU" -> "S"    ,"GGU" -> "G"
	,"UGC" -> "C"    ,"CGC" -> "R"    ,"AGC" -> "S"    ,"GGC" -> "G"
	,"UGA" -> "Stop" ,"CGA" -> "R"    ,"AGA" -> "R"    ,"GGA" -> "G"
	,"UGG" -> "W"    ,"CGG" -> "R"    ,"AGG" -> "R"    ,"GGG" -> "G")

def prot(rna: String): String = {
	rna splitAt 3 match {
		case (codon, rest) => 
		codonTable(codon) match {
			case "Stop" => ""
			case 	x   => x + prot(rest)
		}
	}
}
////////////////////////////////////////////////////////////////////////////////
