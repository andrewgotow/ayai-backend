package ayai.systems

import crane.{EntityProcessingSystem, Entity, World}
import ayai.components._

class HealthSystem() extends EntityProcessingSystem(include=List(classOf[Health], classOf[Character]), exclude=List(classOf[Respawn])) {

  override def processEntity(e : Entity, deltaTime : Int) {
  	val character = e.getComponent(classOf[Character]) match {
  		case(Some(c : Character)) => c
  	}
  	val health = e.getComponent(classOf[Health]) match {
  		case(Some(h : Health)) => h
  	}
  	//look at the status effects of the character

  	if(health.currentHealth <= 0) {
  		//attach respawn to entity
  		e.components += new Respawn(1500, System.currentTimeMillis())
  	}


  }
}