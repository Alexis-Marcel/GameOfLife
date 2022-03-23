object JeuDeLaVie {

  /*
   * Question 1
   */
  type Grille = List[(Int,Int)]

  /**
   * transforme une chaine en une Grille
   */
  def chaineToGrille(s: List[Char], ligne: Int, colonne: Int): Grille = s match {
    case t::q if t==' ' => chaineToGrille(q, ligne, colonne + 1)
    case t::q if t=='X' => (ligne, colonne) :: chaineToGrille(q, ligne, colonne + 1)
    case Nil => Nil
    case _ => throw new IllegalStateException
  }

  /**
   * transforme une liste de chaine en une Grille
   */
  def chainesToGrille(l:List[String]):Grille =  {
    def aux(l:List[String], ligne:Int):Grille = l match {
      case t::q => (chaineToGrille(t.toList, ligne, 0) ++ aux(q, ligne+1))
      case Nil => List[(Int, Int)]()
    }
    aux(l, 0)
  }

  /*
   * Question 2
   */

  /**
   * Cherche le coin minimum et maximum
   * en un parcours
   */
  def coinMinMax(g:Grille):((Int, Int), (Int, Int)) = {
    val (l, c) = g.head
    def nouvelleVal(anc:Int, nouv:Int, comparateur:(Int, Int)=>Boolean) = if(comparateur(anc, nouv)) nouv else  anc
    def nouveauCoin(anc:(Int, Int), nouv:(Int, Int), comparateur:(Int, Int)=>Boolean): (Int, Int)={
      val (ancl, ancc) = anc
      val (nouvl, nouvc) = nouv
      (nouvelleVal(ancl, nouvl, comparateur),  nouvelleVal(ancc, nouvc, comparateur))
    }
    def minMax(acc: ((Int, Int), (Int, Int)), e:(Int, Int)) = {
      val ((lmin, cmin), (lmax, cmax)) = acc
      val (l:Int, c:Int) = e
      (  nouveauCoin((lmin, cmin), (l, c), (a, n)=>a>n)  , nouveauCoin((lmax, cmax), (l, c), (a, n)=>a<n) )
    }
    (g foldLeft ((l, c), (l, c)))(minMax)
  }

  /**
   * Affiche la grille dans la console
   */
  def afficherGrille(g:Grille):Unit = {
    val ((lmin, cmin), (lmax, cmax)) = coinMinMax(g)
    def afficher(ligne:Int, colonne:Int, g:Grille):Unit = g match {
      case _ if ligne > lmax =>
      case v if colonne > cmax => {
        print("\n")
        afficher(ligne+1, cmin, v)
      }
      case t::q =>{
        val (l, c) = t
        if (ligne == l && colonne == c) {
          print("X")
          afficher(ligne, colonne+1, q)
        } else {
          print(" ")
          afficher(ligne, colonne+1, t::q)
        }
      }
      case Nil => {
        print(" ");
        afficher(ligne, colonne+1, Nil)
      }
    }
    afficher(lmin, cmin, g)
  }

  /*
   * Question 3
   */
  def voisines8(l:Int, c:Int):List[(Int, Int)] = {
    List((l-1, c-1),(l-1, c), (l-1, c+1), (l, c-1),(l, c+1), (l+1, c-1), (l+1, c), (l+1, c+1))
  }

  /*
   * Question 4
   */
  def nbVivanteParmiVoisines(l:Int, c:Int, g:Grille):Int={
    val voisines = voisines8(l, c)
    (g foldLeft 0)((acc, e)=> {
      //nbElement a 0 ou 1
      val nbElement = (voisines foldLeft 0)((accV, eV)=> {
        val (lv, cv) = eV
        val (le, ce) = e
        if (le == lv && ce == cv) accV +1
        else accV
      })
      assert(nbElement <= 1, "Doublons dans une liste")
      acc + nbElement
    })
  }
  

}
