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

}
