package com.jamesneve

import org.scalatest._
import org.scalatestplus.play._
import play.api.test._

trait FactoryCleaner extends Suite with BeforeAndAfterEach {
  private def cleanDB = Factory.cleanAllFactories

  override def afterEach() {
    cleanDB
  }
}
