package com.jamesneve

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class FactoryDAO[CaseClassType, IdType](insertDAO: CaseClassType => Future[IdType],
		findByIdDAO: IdType => Future[Option[CaseClassType]],
		deleteByIdDAO: Option[IdType => Future[IdType]] = None) {

	def insert(caseClass: CaseClassType): IdType = {
		Await.result(insertDAO(caseClass), Duration.Inf)
	}

	def findById(id: IdType): CaseClassType = {
		Await.result(findByIdDAO(id), Duration.Inf) match {
			case Some(a) => a
			case None => throw new DBErrorException
		}
	}

	def deleteById(id: IdType): Option[IdType] = {
		deleteByIdDAO match {
			case Some(dao) => Some(Await.result(dao(id), Duration.Inf))
			case None => None
		}
	}
}
