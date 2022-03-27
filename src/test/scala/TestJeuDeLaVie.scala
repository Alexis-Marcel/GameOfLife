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
  

}
