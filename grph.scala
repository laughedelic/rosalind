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
