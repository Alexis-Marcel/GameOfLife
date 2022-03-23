import JeuDeLaVie._;




















//ici c'est pour les "vieux" tests

//exo 1
val liste = List("XX ", " XX", " X ");
chainesToGrille(liste)

val l = List(" XX",
  "  X",
  "XXX")
chainesToGrille(l)


//exo 2
val l2 = List((-1,1),
  (0,1), (1,2), (2,0), (2,1))
coinMinMax(l2)

afficherGrille(l2)
afficherGrille(chainesToGrille(liste))
afficherGrille(chainesToGrille(l))










