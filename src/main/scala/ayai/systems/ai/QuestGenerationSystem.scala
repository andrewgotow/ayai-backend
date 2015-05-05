package ayai.systems

import java.util.Date

import scala.collection.mutable.ListBuffer

import ayai.gamestate.AddQuest
import crane.{Entity, EntityProcessingSystem}

import java.rmi.server.UID

import scala.collection.mutable.ArrayBuffer
import scala.math

/** Akka Imports **/
import akka.actor.{ActorSystem, Props}
import ayai.components._
import ayai.quests._
import ayai.factories.EntityFactory

object QuestGenerationSystem {
  def apply(actorSystem: ActorSystem) = new QuestGenerationSystem(actorSystem)
}

class QuestGenerationSystem(actorSystem: ActorSystem) extends EntityProcessingSystem(include=List(classOf[GenerateQuest])) {
    def processEntity(e: Entity, deltaTime: Int): Unit = {
      println( "ANDREW: Attempting to generate new quest" )

      // fetch the generate quest request on this entity. This request will contain data about the player who needs a quest,
      // and the NPC who will be issuing it.
      e.getComponent(classOf[GenerateQuest]) match {
        case Some(genQuest: GenerateQuest) =>

          // the next thing we'll need is the memory of the quest-giver. we'll use their memories to figure out what the
          // quest should target.
          genQuest.initiator.getComponent(classOf[Memory]) match {
            case Some(initiatorMemory: Memory) =>

              //val recipientHistory = genQuest.recipient.getComponent(classOf[QuestHistory])

              // create an objective list
              var objectives = new ListBuffer[Objective]()

              // next, we need to populate our quest with objectives. To do this, we'll find the most "significant" thing
              // in an NPC's memory and build objectives based on that.
              var mostSignificantMemory: MemoryContents = null
              for (memory <- initiatorMemory.entitiesRemembered) {
                if (math.abs(memory.relationship) > math.abs(mostSignificantMemory.relationship)) {
                  mostSignificantMemory = memory
                }
              }

              // next, we can decide whether that thing is "negative" or "positive". If it's negative, the NPC will want that
              // thing to be killed or something. If it's positive, maybe you should bring it to them.
              if ( mostSignificantMemory != null ) {
                if ( mostSignificantMemory.relationship > 0 ) {
                  // positive relationship quests
                  objectives += new FetchObjective( "Fetch A thing", "Get this", "Bring it here" )
                }else{
                  // negative relationship quests.
                  objectives += new KillObjective( "Get Entity Name from Passed ID Here", 0, 10 )
                }
              }else{
                // this character has literally nothing on his mind... Umm... I guess return null?
                return null
              }

              // so... yeah, sorry about this! Each quest needs an integer ID, preferably one that doesn't conflict with
              // any others. Time's pretty much contiguous, so as long as you don't try and run this game for more than 24 days,
              // at a time, we should be good! Fix this as soon as possible.
              val id = new Date().getTime().toInt

              // create a blank quest which we'll populate in a minute.
              val questComponent = new Quest(
                id, // id
                "hello", // title
                "do a thing", // description
                12, // recommended level
                objectives.toList // objectives
              )

              // add a new quest to the quest bag of the initiator!
              genQuest.initiator.getComponent(classOf[QuestBag]) match {
                case Some(questBag: QuestBag) =>
                  questBag.addQuest(questComponent)
                case _ =>
              }

              // lastly, remove the generation request. We've fulfilled it, so it's no longer needed.
              e.removeComponent(classOf[GenerateQuest])

            case _ =>
          }
        case _ =>
      }
    }
}
