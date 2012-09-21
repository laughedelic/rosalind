object common {
	
	def updMap[K,V](m: Map[K,V], key: K, f: V => V): Map[K,V] =
	if(m contains key) m + (key -> f(m(key))) else m

}
