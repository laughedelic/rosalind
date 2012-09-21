//// Counting Bases: http://rosalind.info/problems/acgu/ ///////////////////////
// Given: An RNA string s of length at most 1000 nt.
// Return: Four integers corresponding to the number of times 
//		   that the symbols A, C, G, and U occur in s.

import common._

def countBases(rna: String): Map[Char, Int] = {
	val bases = Map('A' -> 0, 'C' -> 0, 'G' -> 0, 'U' -> 0)
	rna.foldLeft(bases)( (acc, x) => updMap(acc, x, (_:Int) + 1) )
}

def acgu(rna: String): String = countBases(rna).values.mkString(" ")
