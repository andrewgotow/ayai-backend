package ayai.components

import crane.Component
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

/** External Imports **/
import scala.collection.mutable.ArrayBuffer

case class QuestBag(quests: ArrayBuffer[Quest] = new ArrayBuffer[Quest]()) extends Component {
  val typename = "quest-offer"
  def asJson(): JObject = {
    ("quests" -> quests.map{quest => quest.asJson})
  }

  def addQuest(questToAdd: Quest): Unit = {
    if (questToAdd != null) {
      println("Quest with ID: " + questToAdd.id + " Added to Bag" )
      println( "   " + questToAdd.asJson() )

      quests += questToAdd
    }
  }

  def removeQuest(id: Int) : Option[Quest] = {
    println("Quest with ID: " + id + " Removed from Bag" )

    val index = quests.indexWhere( _.id == id )
    if ( index >= 0 ) Some(quests.remove( index )) else None
  }

}