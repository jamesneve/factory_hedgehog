package com.jamesneve.factoryhedgehog

import scala.reflect._
import scala.reflect.runtime.universe._
import java.lang.reflect.InvocationTargetException

class Factory[CaseClassType, IdType](obj: CaseClassType,
		typeTag: Type,
		factoryDao: Option[FactoryDAO[CaseClassType, IdType]] = None, 
		associations: Option[Vector[(String, String)]] = None) {

	val createdObjects = scala.collection.mutable.ListBuffer[IdType]()

	private def build: CaseClassType = obj

	private def buildWithValues(m: Map[String, Any]): CaseClassType = {
		if(m != null) {
			val mm = collection.mutable.Map(m.toSeq: _*)
			val mappedObject = dematerialise(obj)
			for(tup <- mm) mappedObject += tup
			materialise(mappedObject, obj)
		} else build
	}

	private def insertMappedObject(obj: CaseClassType): CaseClassType = {
		factoryDao match {
			case Some(dao) => {
				createdObjects += dao.insert(obj)
				obj
			}
			case None => throw new UndefinedDaoException
		}
	}

	private def create: CaseClassType = {
		factoryDao match {
			case Some(dao) => buildAssociationTree(dematerialise(obj))
			case None => throw new UndefinedDaoException
		}
	}

	private def createWithValues(m: Map[String, Any]): CaseClassType = {
		factoryDao match {
			case Some(dao) => {
				if(m != null) {
					val mm = collection.mutable.Map(m.toSeq: _*)
					val mappedObject = dematerialise(obj)
					for(tup <- mm) mappedObject += tup
					buildAssociationTree(mappedObject)
				} else create
			}
			case None => throw new UndefinedDaoException
		}
	}

	private def cleanCreatedObjects: Unit = {
		while(!createdObjects.isEmpty) {
			val newId = createdObjects.head
			delete(newId)
			createdObjects -= newId
		}
	}

	private def delete(id: IdType): Boolean = {
		factoryDao match {
			case Some(dao) => {
				dao.deleteById(id) match {
					case Some(a) => true
					case None => throw new InvalidDataException
				}
			}
			case None => throw new UndefinedDaoException
		}
	}

	private def buildAssociationTree(mappedObject: scala.collection.mutable.Map[String, Any]): CaseClassType = {
		associations match {
			case None => insertMappedObject(materialise(mappedObject, obj))
			case Some(a) => {
				for(association <- a) {
					val foreignObjectFactory = Factory.get(association._1)
					val mappedForeignObject = dematerialise(foreignObjectFactory.build)
					mappedObject += (association._2 -> foreignObjectFactory.buildAssociationTree(mappedForeignObject))
				}
				insertMappedObject(materialise(mappedObject, obj))
			}
		}
	}

	private def dematerialise(cc: Any): scala.collection.mutable.Map[String, Any] = {
		(scala.collection.mutable.Map[String, Any]() /: cc.getClass.getDeclaredFields) { (a, f) =>
	    f.setAccessible(true)
	    a + (f.getName -> f.get(cc))
	  }
	}

  private def materialise(m: scala.collection.mutable.Map[String,Any], o: CaseClassType) : CaseClassType = {
  	try {
	    val rm = runtimeMirror(o.getClass.getClassLoader)

	    val classTest = typeTag.typeSymbol.asClass
	    val constructor = typeTag.decl(termNames.CONSTRUCTOR).asMethod
	    val classMirror = rm.reflectClass(classTest)
	    val constructorMirror = classMirror.reflectConstructor(constructor)

	    val constructorArgs = constructor.paramLists.flatten.map( (param: Symbol) => {
	      val paramName = param.name.toString
	      if(param.typeSignature <:< typeOf[Option[Any]]) {
	        m.get(paramName).get
	      } else {
	        m.get(paramName).getOrElse(throw new IllegalArgumentException("Map is missing required parameter named " + paramName))
	      }
	    })

	    constructorMirror(constructorArgs:_*).asInstanceOf[CaseClassType]
    } catch {
    	case e: InvocationTargetException => if(e.getCause() != null) throw e.getCause() else throw e
    }
  }
}

object Factory {
	private val factories = scala.collection.mutable.Map[String, Factory[_, _]]()

	def add(name: String, factory: Factory[_, _]) = {
		if(!factories.contains(name)) factories += (name -> factory)
	}

	def get(name: String): Factory[_, _] = {
		if(factories.contains(name)) factories(name)
		else throw new NoFactoryException
	}

	def build(name: String, values: Map[String, Any] = null) = {
		if(factories.contains(name)) {
			if(values == null) factories(name).build
			else factories(name).buildWithValues(values)
		} else throw new NoFactoryException
	}

	def create(name: String, values: Map[String, Any] = null) = {
		if(factories.contains(name)) {
			if(values == null) factories(name).create
			else factories(name).createWithValues(values)
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
