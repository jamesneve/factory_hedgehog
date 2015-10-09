package com.jamesneve.factoryhedgehog

import org.scalatest._
import org.scalatestplus.play._
import play.api.test._

trait CleanerHedgehog extends Suite with BeforeAndAfterEach {
  private def cleanDB = Factory.cleanAllFactories

  override def afterEach() {
    cleanDB
  }
}
