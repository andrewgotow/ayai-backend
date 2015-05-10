package ayai.systems

import java.util.Date

import scala.collection.mutable.ListBuffer

import ayai.gamestate.AddQuest
import crane.{Entity, EntityProcessingSystem}
import scala.util.Random
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
              println("ANDREW: Retrieving most significant memory for NPC")
              val mostSignficantMemory: Option[MemoryContents] = {
                if (initiatorMemory.entitiesRemembered.nonEmpty) {
                  Some(initiatorMemory.entitiesRemembered.maxBy(memory => math.abs(memory.relationship)))
                } else {
                  None
                }
              }

              // next, we can decide whether that thing is "negative" or "positive". If it's negative, the NPC will want that
              // thing to be killed or something. If it's positive, maybe you should bring it to them for now. This will be updated
              // to select a random quest type, weighted by player preferences in the future, but for testing it's just the two.
              println("ANDREW: Building objectives based on significant memories")
              mostSignficantMemory.map(memory => {
                // this is how unique entities are found in other parts of the codebase. I'm not really sure why its
                // done through tags, rather than just having a "getEntityById", but
                val targetID = memory.entityID
                val target = e.world.get.getEntityByTag(s"$targetID")

                if (memory.relationship > 0) {
                  // positive relationship quests
                  // for now, the fetch target is the initiator themself, as the remembered entity isn't stored with a string ID that we can directly plug in.
                  objectives += new FetchObjective("Fetch TARGET_NAME", /*Memory ID should be a string memory.entityID*/ genQuest.initiator.uuid, genQuest.initiator.uuid)
                } else {
                  // negative relationship quests.
                  objectives += new KillObjective("Eliminate TARGET_NAME", 0, 10)
                }
              })

              // so... yeah, sorry about this! Each quest needs an integer ID, preferably one that doesn't conflict with
              // any others. For now, this solution just chooses a random integer. For most purposes, the odds of a collision
              // is incredibly slim, and it should function just fine. Note however that there is no guarantee that each ID is
              // unique, and if this game were actually run on a server for a long time, a collision would be likely.
              val id = new Random().nextInt(Int.MaxValue - 1)

              // create a blank quest which we'll populate in a minute.
              val questComponent = new Quest(
                id, // id
                "hello", // title
                "do a thing", // description
                12, // recommended level
                objectives.toList // objectives
              )

              // add a new quest to the quest bag of the initiator!
              print("ANDREW: Adding quest to quest bag")
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
