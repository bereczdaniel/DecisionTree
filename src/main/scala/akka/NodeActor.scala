package akka

import akka.NodeActor._
import akka.actor.{ActorRef, FSM, Props}
import instance.Features


/**
  * Skeleton for Akka based decision tree
  * @param instances: Data
  */
class NodeActor(instances: Instances) extends FSM[State, Data] {

  startWith(Leaf, instances)
  initialize()

  when(Leaf) {
    case Event(msg: Predict, data: Instances) =>
      stay()
    case Event(msg: Split, data: Instances) =>
      val left = context.actorOf(NodeActor.props(data))
      val right = context.actorOf(NodeActor.props(data))
      goto(InternalNode) using Children(left, right, _ => true)

    case Event(CountLeafs(), _) =>
      sender ! 1
      stay()
  }

  when(InternalNode) {
    case Event(msg: Predict, Children(left, right, rule)) =>
      if(rule(msg.f)){
        left ! msg
      }
      else
        right ! msg

      stay()

    case Event(x: Int, _) =>
      context.parent ! x
      context.children.size
      stay()
  }

}

object NodeActor {
  case class Insert()
  case class Predict(f: Features)
  case class Split()
  case class CountLeafs()

  sealed trait State
  case object Leaf extends State
  case object InternalNode extends State

  sealed trait Data
  case class Instances() extends Data
  case class Children(left: ActorRef, right: ActorRef, rule: Features => Boolean) extends Data

  def props(instances: Instances): Props  = Props(new NodeActor(instances))
}