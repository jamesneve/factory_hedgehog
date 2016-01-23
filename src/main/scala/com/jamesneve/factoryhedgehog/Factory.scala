package com.jamesneve.factoryhedgehog

import scala.reflect._
import scala.reflect.runtime.universe._
import java.lang.reflect.InvocationTargetException

class Factory[CaseClassType, IdType](obj: CaseClassType,
		typeTag: Type,
		factoryDao: Option[FactoryDAO[CaseClassType, IdType]] = None, 
		associations: Option[Vector[(String, String)]] = None) {

	val createdObjects = scala.collection.mutable.ListBuffer[IdType]()

	 def build: CaseClassType = obj

	 def buildWithValues(m: Map[String, Any]): CaseClassType = {
		if(m != null) {
			val mm = collection.mutable.Map(m.toSeq: _*)
			val mappedObject = dematerialise(obj)
			for(tup <- mm) mappedObject += tup
			materialise(mappedObject, obj)
		} else build
	}

	 def insertMappedObject(obj: CaseClassType): IdType = {
		factoryDao match {
			case Some(dao) => {
				val finalObjectId = dao.insert(obj)
				createdObjects += finalObjectId
				finalObjectId
			}
			case None => throw new UndefinedDaoException
		}
	}

	 def create: CaseClassType = {
		factoryDao match {
			case Some(dao) => dao.findById(buildAssociationTree(dematerialise(obj)))
			case None => throw new UndefinedDaoException
		}
	}

	 def createWithValues(m: Map[String, Any]): CaseClassType = {
		factoryDao match {
			case Some(dao) => {
				if(m != null) {
					val mm = collection.mutable.Map(m.toSeq: _*)
					val mappedObject = dematerialise(obj)
					for(tup <- mm) mappedObject += tup
					dao.findById(buildAssociationTree(mappedObject))
				} else create
			}
			case None => throw new UndefinedDaoException
		}
	}

	 def cleanCreatedObjects: Unit = {
		while(!createdObjects.isEmpty) {
			val newId = createdObjects.head
			delete(newId)
			createdObjects -= newId
		}
	}

	 def delete(id: IdType): Boolean = {
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

	def buildAssociationTree(mappedObject: scala.collection.mutable.Map[String, Any]): IdType = {
		associations match {
			case None => insertMappedObject(materialise(mappedObject, obj))
			case Some(a) => {
				for(association <- a) {
					val foreignObjectFactory = FactoryHedgehog.get(association._1)
					val mappedForeignObject = dematerialise(foreignObjectFactory.build)
					mappedObject += (association._2 -> foreignObjectFactory.buildAssociationTree(mappedForeignObject))
				}
				insertMappedObject(materialise(mappedObject, obj))
			}
		}
	}

	 def dematerialise(cc: Any): scala.collection.mutable.Map[String, Any] = {
		(scala.collection.mutable.Map[String, Any]() /: cc.getClass.getDeclaredFields) { (a, f) =>
	    f.setAccessible(true)
	    a + (f.getName -> f.get(cc))
	  }
	}

   def materialise(m: scala.collection.mutable.Map[String,Any], o: CaseClassType) : CaseClassType = {
  	try {
	    val rm = runtimeMirror(o.getClass.getClassLoader)

	    val classTest = typeTag.typeSymbol.asClass
	    val constructor = typeTag.decl(termNames.CONSTRUCTOR).asMethod
	    val classMirror = rm.reflectClass(classTest)
	    val constructorMirror = classMirror.reflectConstructor(constructor)

	    val constructorArgs = constructor.paramLists.flatten.map( (param: Symbol) => {
	      val paramName = param.name.toString
	      if(param.typeSignature <:< typeOf[Option[Any]]) {
	        val tentativeInsert = m.get(paramName).get
	        tentativeInsert match {
	        	case a: Option[_] => a
	        	case b: Any => Some(b)
	        }
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
