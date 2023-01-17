package typeclassderivation

import munit.FunSuite
import typeclassderivation.shapeless3.Show

class ShowDerivedInstancesSpec extends FunSuite {

  final case class Foo(i: Int, s: String, b: Boolean)

  enum ColorEnum:
    case Red, Green, Blue

  sealed trait Color
  object Color:
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color

  enum XEnum:
    case Y(i: Int, s: String, b: Boolean)
    case Z

  sealed trait X
  object X:
    final case class Y(i: Int, s: String, b: Boolean) extends X
    case object Z extends X

  test("Type class instances derived using Shapeless 3") {

    import typeclassderivation.shapeless3.Show

    case class Bar(i: Int, s: String, b: Boolean) derives Show

    assertEquals(Show[Bar].show(Bar(1, "s", true)), "Bar(i = 1, s = s, b = true)")
    assertEquals(Show[Foo].show(Foo(1, "s", true)), "Foo(i = 1, s = s, b = true)")
    assertEquals(Show[ColorEnum].show(ColorEnum.Blue), "Blue()")
    assertEquals(Show[Color].show(Color.Blue), "Blue()")
    assertEquals(
      Show[XEnum].show(XEnum.Y(1, "s", true)),
      "Y(i = 1, s = s, b = true)"
    )
    assertEquals(
      Show[XEnum].show(XEnum.Z),
      "Z()"
    )
    assertEquals(
      Show[X].show(X.Y(1, "s", true)),
      "Y(i = 1, s = s, b = true)"
    )

  }

  test("Type class instances derived using only Scala 3") {

    import typeclassderivation.scala3.Show

    case class Bar(i: Int, s: String, b: Boolean) derives Show

    assertEquals(Show[Bar].show(Bar(1, "s", true)), "Bar(i = 1, s = s, b = true)")
    assertEquals(Show[Foo].show(Foo(1, "s", true)), "Foo(i = 1, s = s, b = true)")
    assertEquals(Show[ColorEnum].show(ColorEnum.Blue), "Blue()")
    assertEquals(Show[Color].show(Color.Blue), "Blue()")
    assertEquals(
      Show[XEnum].show(XEnum.Y(1, "s", true)),
      "Y(i = 1, s = s, b = true)"
    )
    assertEquals(
      Show[XEnum].show(XEnum.Z),
      "Z()"
    )
    assertEquals(
      Show[X].show(X.Y(1, "s", true)),
      "Y(i = 1, s = s, b = true)"
    )
    assertEquals(
      Show[X].show(X.Z),
      "Z()"
    )

  }

}