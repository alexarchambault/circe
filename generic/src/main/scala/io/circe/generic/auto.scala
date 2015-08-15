package io.circe.generic

/**
 * Fully automatic codec derivation.
 *
 * Importing the contents of this object provides [[io.circe.Decoder]] and [[io.circe.Encoder]]
 * instances for tuples, case classes (if all members have instances), "incomplete" case classes,
 * sealed trait hierarchies, tuples, etc.
 */
object auto
  extends SingletonInstances
  with DerivedInstances
  with DefaultProductCodec
  with DefaultSumCodec
  with IncompleteInstances
