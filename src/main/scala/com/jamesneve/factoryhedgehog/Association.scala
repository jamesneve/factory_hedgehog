package com.jamesneve.factoryhedgehog

case class Association(foreign_key: String, factoryName: String, autoCreate: Boolean = true)
