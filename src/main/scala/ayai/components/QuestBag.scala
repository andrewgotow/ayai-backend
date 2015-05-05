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

  def removeQuest(id: Int) : Quest = {
    println("Quest with ID: " + id + " Removed from Bag" )

    for(quest <- quests) {
      if (quest.id == id) {
        return quests.remove( quests.indexOf(quest) )
      }
    }
    return null
  }

  /*
  def dequeueQuest() : Quest = {
    println( "ANDREW: Deququing quest" );
    // if our quest bag is empty, just return null for now.
    if ( quests.isEmpty ) {
      return null
    }

    // then just pop the first one off the queue.
    return quests.remove(0);
  }*/

}