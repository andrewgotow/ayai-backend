package ayai.quests

import net.liftweb.json.JsonDSL._
import net.liftweb.json._

class KillObjective(name: String, totalCompleted: Int, totalNeeded:Int) extends Objective(name) {
	override def asJson(): JObject = {
		("name" -> name) ~
    //("targetTag" -> targetTag) ~
		("totalCompleted" -> totalCompleted) ~
		("totalNeeded" -> totalNeeded)
	}

  override def isComplete: Boolean = {
    totalCompleted >= totalNeeded
  }
}