package org.jetbrains.plugins.scala.testingSupport.scalatest.singleTest.tagged

import org.jetbrains.plugins.scala.testingSupport.scalatest.generators.FlatSpecGenerator
import org.jetbrains.plugins.scala.testingSupport.scalatest.singleTest.FlatSpecSingleTestTestBase

trait FlatSpecTaggedSingleTestTest extends FlatSpecSingleTestTestBase with FlatSpecGenerator {

  val flatSpecTaggedTestPath = TestNodePath("[root]", flatSpecClassName, "A FlatSpecTest", "should run tagged tests")

  def testTaggedFlatSpec(): Unit = {
    doTest(flatSpecClassName)(loc(flatSpecFileName, 18, 7))(
      "A FlatSpecTest should run tagged tests",
      flatSpecTaggedTestPath
    )
  }
}
