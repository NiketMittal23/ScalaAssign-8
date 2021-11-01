import com.orderedTrait.{Empty, Input}
import org.scalatest.funsuite.AnyFunSuite

class TestOrderedTrait extends AnyFunSuite{

  val nonEmpty = new Empty[Input].incl(Input(1)).incl(Input(2))

  test("is set contain the value?? then Pass"){
    assert(nonEmpty.contains(Input(1)))
  }

  test("if not contain then pass"){
    assert(!nonEmpty.contains(Input(9)))
  }

}
