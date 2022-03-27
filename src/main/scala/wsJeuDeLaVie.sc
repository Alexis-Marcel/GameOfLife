import JeuDeLaVie._;

val liste = List("XX ", " XX", " X ");
val l = List(" XX", "  X", "XXX")
val l2 = List((-1,1),(0,1), (1,2), (2,0), (2,1))



afficherGrille(l2)
afficherGrille(survivantes(l2))
afficherGrille(naissances(l2))

naissances(l2).length
candidates(l2).length

val listeCligno = List((0,-1), (0,0), (0,1))
val clownEn110 = List((-1,-1), (-1,0), (-1 ,1), (0,-1), (0 , 1),(1,-1), (1, 1) )

val autoFredkin = List((0, 0))
afficherGrille(autoFredkin)

automateDeFredkin(autoFredkin, 5)

afficherGrille(voisines4(0,0))
afficherGrille(((l:Int, c:Int)=>List((l-1, c-1), (l-1, c+1),(l+1, c-1), (l+1, c+1)))(0,0))

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
nbVivanteParmiVoisines(voisines8(0, 1),l2)

survivantes(l2)
survivantes(chainesToGrille(l))
survivantes(chainesToGrille(liste))

//exo5
candidates(l2)
//exo 6
naissances(l2)

//exo 7
clownEn110
afficherGrille(clownEn110)
jeuDeLaVieQuestion7(clownEn110, 110 )
