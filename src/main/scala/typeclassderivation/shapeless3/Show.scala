package typeclassderivation.shapeless3

import shapeless3.deriving.*
import typeclassderivation.scala3.Show

trait Show[A]:
  def show(a: A): String

object Show:

  def apply[A](using inst: Show[A]): Show[A] = inst

  given Show[Int] = _.toString
  given Show[Boolean] = _.toString
  given Show[String] = identity(_)

  def deriveShowProduct[A](using
      inst: K0.ProductInstances[Show, A],
      labelling: Labelling[A]
  ): Show[A] =
    (a: A) =>
        labelling.elemLabels.zipWithIndex
          .map { (label, index) =>
            s"$label = ${inst.project(a)(index)([t] => (st: Show[t], pt: t) => st.show(pt))}"
          }
          .mkString(s"${labelling.label}(", ", ", ")")

  def deriveShowSum[A](using
      inst: K0.CoproductInstances[Show, A]
  ): Show[A] =
    (a: A) => inst.fold(a)([a] => (st: Show[a], a: a) => st.show(a))

  inline given derived[A](using gen: K0.Generic[A]): Show[A] =
    gen.derive(deriveShowProduct, deriveShowSum)
