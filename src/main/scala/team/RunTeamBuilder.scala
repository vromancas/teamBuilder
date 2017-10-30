package team

object RunTeamBuilder extends App {
  val tb = new TeamBuilder()
  println(tb.specialLocations(Array("0010","1000","1100","1000")))
}