import JeuDeLaVie._;
import org.scalatest.FunSuite;

class TestJeuDeLaVie extends FunSuite {
  test("chainesToGrille.normal") {
    val liste = List("XX ", " XX", " X ");
    val res = chainesToGrille(liste)
    val exp = List((0,0), (0,1), (1,1), (1,2), (2,1))
    assert(res === exp)
  }
}
