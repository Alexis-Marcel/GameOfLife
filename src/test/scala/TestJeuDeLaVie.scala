import JeuDeLaVie._;
import org.scalatest.FunSuite;

class TestJeuDeLaVie extends FunSuite {

  /*
   * Question 1
   */

  test("chainesToGrille.normal") {
    val liste = List("XX ", " XX", " X ");
    val res = chainesToGrille(liste)
    val exp = List((0,0), (0,1), (1,1), (1,2), (2,1))
    assert(res === exp)
  }

  test("chainesToGrille.grand") {
    val liste = List("XX   X", " XX   ", "X   X ");
    val res = chainesToGrille(liste)
    val exp = List((0,0), (0,1),(0,5), (1,1), (1,2), (2,0), (2,4))
    assert(res === exp)
  }

  /*
   * Question 2
  */

  test("coinMinMax.normal") {
    val liste = List((0,0), (0,1),(0,5), (1,1), (1,2), (2,0), (2,4))
    val exp  =((0,0), (2,5))
    val res = coinMinMax(liste)
    assert(res === exp)
  }

  test("coinMinMax.inverse") {
    val liste = List((0,0), (0,1),(0,5), (1,1), (1,2), (2,0), (2,4)).reverse
    val exp  =((0,0), (2,5))
    val res = coinMinMax(liste)
    assert(res === exp)
  }

  test("coinMinMax.grand") {
    val liste = List((0,0), (0,1),(0,5), (1,1), (1,2), (2,0), (2,4), (Integer.MAX_VALUE, Integer.MIN_VALUE), (Integer.MIN_VALUE, Integer.MAX_VALUE) )
    val exp  =((Integer.MIN_VALUE,Integer.MIN_VALUE), (Integer.MAX_VALUE,Integer.MAX_VALUE))
    val res = coinMinMax(liste)
    assert(res === exp)
  }

  /*
   * Question 3
  */

  test("voisines8.normal") {
    val exp2 = List((-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1))
    val res2 = voisines8( 0 , 0)
    assert(res2 === exp2)
    assert(res2.length == 8)

    val exp  = List((-7,-2), (-7,-1), (-7,0), (-6,-2), (-6,0), (-5,-2), (-5,-1), (-5,0))
    val res = voisines8( -6 , -1)
    assert(res === exp)
    assert(res.length == 8)
  }

  test("voisines8.grandOuPetit") {
    intercept[IndexOutOfBoundsException](voisines8( Integer.MAX_VALUE , 0))
    intercept[IndexOutOfBoundsException](voisines8( 0 , Integer.MAX_VALUE))
    intercept[IndexOutOfBoundsException](voisines8( Integer.MIN_VALUE , 0))
    intercept[IndexOutOfBoundsException](voisines8( 0, Integer.MIN_VALUE ))
    intercept[IndexOutOfBoundsException](voisines8( Integer.MAX_VALUE, Integer.MIN_VALUE ))
  }

}
