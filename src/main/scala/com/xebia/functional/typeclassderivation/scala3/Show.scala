package com.xebia.functional.typeclassderivation.scala3

import com.xebia.functional.typeclassderivation.shapeless3.Show
import shapeless3.deriving.K0

import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror

trait Show[A]:
  def show(a: A): String

object Show:

  def apply[A](using inst: Show[A]): Show[A] = inst

  given Show[Int] = _.toString
  given Show[Boolean] = _.toString
  given Show[String] = identity(_)

  inline def summonAll[T <: Tuple]: List[Show[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[Show[t]] :: summonAll[ts]

  inline def summonLabels[Values <: Tuple]: List[String] =
    inline erasedValue[Values] match {
      case _: (elem *: elems1) =>
        constValue[elem].toString :: summonLabels[elems1]
      case _ => Nil
    }

  def deriveShowProduct[T](
      label: String,
      shows: => List[Show[_]],
      labels: => List[String]
  ): Show[T] = (t: T) =>
    t.asInstanceOf[Product]
      .productIterator
      .zip(shows.iterator)
      .zip(labels)
      .map { case ((productElement, showInstance), label) =>
        s"$label = ${showInstance.asInstanceOf[Show[Any]].show(productElement)}"
      }
      .mkString(s"$label(", ", ", ")")

  private def deriveShowSum[T](
      s: Mirror.SumOf[T],
      shows: => List[Show[_]]
  ): Show[T] =
    (t: T) =>
      val index = s.ordinal(t)
      shows(index).asInstanceOf[Show[Any]].show(t)

  inline given derived[T](using m: Mirror.Of[T]): Show[T] =
    lazy val showsInstances = summonAll[m.MirroredElemTypes]
    lazy val labels = summonLabels[m.MirroredElemLabels]
    val typeLabel = constValue[m.MirroredLabel]
    inline m match
      case s: Mirror.SumOf[T] => deriveShowSum(s, showsInstances)
      case _: Mirror.ProductOf[T] =>
        deriveShowProduct(typeLabel, showsInstances, labels)
