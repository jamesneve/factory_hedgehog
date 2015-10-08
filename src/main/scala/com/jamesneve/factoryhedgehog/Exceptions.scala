package com.jamesneve

class NoFactoryException(msg: String = "No factory with that name exists.") extends RuntimeException(msg)
class DBErrorException(msg: String = "There was a problem with inserting the record. Your DB table schema is probably inconsistent with the object you provided.") extends RuntimeException(msg)
class NoForeignKeyException(msg: String = "No foreign key with that value exists.") extends RuntimeException(msg)
class InvalidDataException(msg: String = "There's a problem with the values you're trying to insert or delete or the object's associations. Possible cause: If you want to use FactoryCleaner, all factories must have a delete method defined.") extends RuntimeException(msg)
class UndefinedDaoException(msg: String = "Please pass a FactoryDAO object to the factory if you want to use the database.") extends RuntimeException(msg)
