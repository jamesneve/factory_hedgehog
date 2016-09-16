package com.jamesneve.factoryhedgehog

import scala.reflect._
import scala.reflect.runtime.universe._

object FactoryHedgehog {
  private val factories = scala.collection.mutable.Map[String, Factory[_, _]]()

  def add[T: TypeTag, U](name: String, obj: T, factoryDao: Option[FactoryDAO[T, U]] = None, associations: Option[Vector[(String, String)]] = None): Unit = {
    factories += (name -> new Factory(obj, typeOf[T], factoryDao, associations))
  }

  def add(name: String, factory: Factory[_, _]): Unit = {
    println("Creating factories by 'new Factory' is depreciated. Please read the documentation for the new way of creating factories.")
    if(!factories.contains(name)) factories += (name -> factory)
    else throw new FactoryExistsException
  }

  def get(name: String): Factory[_, _] = {
    if(factories.contains(name)) factories(name)
    else throw new NoFactoryException
  }

  def build[T: ClassTag](name: String, values: Map[String, Any] = null): T = {
    if(factories.contains(name)) {
      if(values == null) factories(name).build.asInstanceOf[T]
      else factories(name).buildWithValues(values).asInstanceOf[T]
    } else throw new NoFactoryException
  }

  def create[T: ClassTag](name: String, values: Map[String, Any] = null): T = {
    if(factories.contains(name)) {
      if(values == null) factories(name).create.asInstanceOf[T]
      else factories(name).createWithValues(values).asInstanceOf[T]
    } else throw new NoFactoryException
  }

  def cleanFactory(name: String): Unit = {
    if(factories.contains(name)) {
      factories(name).cleanCreatedObjects
    } else throw new NoFactoryException
  }

  def cleanAllFactories: Unit = {
    for(a <- factories) a._2.cleanCreatedObjects
  }
}
