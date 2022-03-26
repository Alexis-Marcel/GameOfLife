import JeuDeLaVie._;

val l = List(" XX", "  X", "XXX")
val l2 = List((-1,1),(0,1), (1,2), (2,0), (2,1))
val liste = List("XX ", " XX", " X ");



jeuDeLaVie(l2, 12)

l2

candidates(l2)
naissances(l2)

nbVivanteParmiVoisines(0, 1, l2)

survivantes(l2)
survivantes(chainesToGrille(l))
survivantes(chainesToGrille(liste))


afficherGrille(l2 )
afficherGrille(candidates(l2))



//ici c'est pour les "vieux" tests


//exo 1
chainesToGrille(liste)

chainesToGrille(l)


//exo 2
coinMinMax(l2)

afficherGrille(l2)
afficherGrille(chainesToGrille(liste))
afficherGrille(chainesToGrille(l))

//exo 3
voisines8( -6 , -1)
voisines8( 2 , 4).length
voisines8( 2 , -1)

//exo 4
nbVivanteParmiVoisines(0, 1, l2)

survivantes(l2)
survivantes(chainesToGrille(l))
survivantes(chainesToGrille(liste))






