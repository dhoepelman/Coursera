package stackoverflow

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("Scored") {

    val xs =
      """((1,6,None,None,140,Some(CSS)),67)
        |((1,42,None,None,155,Some(PHP)),89)
        |((1,72,None,None,16,Some(Ruby)),3)
        |((1,126,None,None,33,Some(Java)),30)
        |((1,174,None,None,38,Some(C#)),20)""".stripMargin.split("\n").toSet

    val matching = stackoverflow.StackOverflow.scored.map(_.toString.replace("Posting", "")).filter(xs.contains).collect().toSet

    println(matching)

    assert(xs == matching, "Different entries")
    assert(stackoverflow.StackOverflow.scored.count() == 2121822, "Scored count")
  }

  test("Vectors") {
    val xs =
      """(350000,67)
        |(100000,89)
        |(300000,3)
        |(50000,30)
        |(200000,20)""".stripMargin.split("\n").toSet

    val matching = stackoverflow.StackOverflow.vectors.map(_.toString).filter(xs.contains).collect().toSet

    println(matching)

    assert(xs == matching, "Different entries")
  }


}
