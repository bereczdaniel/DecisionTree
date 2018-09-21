package akka

import akka.NodeActor._
import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.event.{Logging, LoggingAdapter}
import akka.testkit.{TestFSMRef, TestProbe}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

import scala.concurrent.ExecutionContext


class NodeActorTest extends FlatSpec with Matchers with BeforeAndAfterAll with Eventually with ScalaFutures{
  implicit val system: ActorSystem = ActorSystem("test")

  implicit val log: LoggingAdapter = Logging(system, this.getClass)
  implicit val ec: ExecutionContext = system.dispatcher


  def cleanup(actors: ActorRef*): Unit = {
    val probe = TestProbe()
    actors.foreach { actor =>
      probe watch actor
      actor ! PoisonPill
      probe.expectTerminated(actor)
    }
  }

  lazy val fsm = TestFSMRef(new NodeActor(Instances()), s"node-actor")

  "NodeActor " should "start in Leaf" in {
    fsm.stateName shouldBe Leaf
    fsm.stateData shouldBe Instances()

    fsm ! Split
    fsm.stateName shouldBe InternalNode
  }

  "NodeActor" should "go to InternalNode" in {
    fsm ! CountLeafs
    fsm.stateName shouldBe InternalNode
  }

}
