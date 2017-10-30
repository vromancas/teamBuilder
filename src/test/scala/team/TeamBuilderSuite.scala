package team

import org.scalatest.FunSuite

class TeamBuilderSuite extends FunSuite {
  
  val tb = new TeamBuilder()
  
  test("""case "010","000","110" """) {
    assert(tb.specialLocations(Array("010","000","110")).mkString == "01")
  }
  
  test("""case "0010","1000","1100","1000" """) {
    assert(tb.specialLocations(Array("0010","1000","1100","1000")).mkString == "03")
  }
    
  test("""case "01000","00100","00010","00001","10000" """) {
    assert(tb.specialLocations(Array("01000","00100","00010","00001","10000")).mkString == "05")
  }
  
  test("""case "0110000","1000100","0000001","0010000","0110000","1000010","0001000" """) {
    assert(tb.specialLocations(Array("0110000","1000100","0000001","0010000","0110000","1000010","0001000")).mkString == "03")
  }
}

