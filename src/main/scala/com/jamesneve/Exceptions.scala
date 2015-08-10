package com.jamesneve

class DuplicateFactoryException(msg: String = "You can't have two factories with the same name.") extends RuntimeException(msg)
class NoFactoryException(msg: String = "No factory with that name exists.") extends RuntimeException(msg)
class DBErrorException(msg: String = "There was a problem with inserting the record. Your DB table schema is probably inconsistent with the object you provided.") extends RuntimeException(msg)
class NoForeignKeyException(msg: String = "No foreign key with that value exists.") extends RuntimeException(msg)
class InvalidDataException(msg: String = "There's a problem with the values you're trying to insert or the object's associations.") extends RuntimeException(msg)
