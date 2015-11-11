package io.circe

import io.circe.singletons._

import io.circe.tests.{ CodecTests, CirceSuite }
import org.scalacheck.{ Arbitrary, Gen }

import algebra.Eq
import shapeless.{ Witness => W }
import shapeless.syntax.singleton._

trait ExtraInstances {

  implicit def arbitrarySingletonType[S](implicit w: W.Aux[S]): Arbitrary[S] =
    Arbitrary(Gen.const(w.value))

  implicit def eqSingletonType[S](implicit w: W.Aux[S]): Eq[S] =
    Eq.fromUniversalEquals

}

class SingletonSuite extends CirceSuite with ExtraInstances {

  checkAll("Codec[W.`4`.T]", CodecTests[W.`4`.T].codec)
  checkAll("""Codec[W.`"ab"`.T]""", CodecTests[W.`"ab"`.T].codec)
  checkAll("Codec[W.`false`.T]", CodecTests[W.`false`.T].codec)

}

class SingletonFieldsSuite extends CirceSuite with ExtraInstances {
  import io.circe.generic.auto._

  case class SingletonFields(
    n: W.`5`.T,
    s: W.`"test"`.T,
    other: Double
  )

  object SingletonFields {
    implicit val arbitrary: Arbitrary[SingletonFields] = Arbitrary(
      for {
        other <- Arbitrary.arbitrary[Double]
      } yield SingletonFields(5.narrow, "test".narrow, other)
    )
    implicit val eq: Eq[SingletonFields] = Eq.fromUniversalEquals
  }

  checkAll("Codec[SingletonFields]", CodecTests[SingletonFields].codec)

}
