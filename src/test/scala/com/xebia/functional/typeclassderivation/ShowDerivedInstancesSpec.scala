package com.xebia.functional.typeclassderivation

import munit.FunSuite

class ShowDerivedInstancesSpec extends FunSuite {

  final case class Foo(x: Int, y: String, z: Boolean)

  enum ColorEnum:
    case Red, Green, Blue

  sealed trait Color
  object Color:
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color

  enum StatusEnum:
    case Good(score: Int)
    case Bad(reason: String)

  sealed trait Status
  object Status:
    final case class Good(score: Int) extends Status
    final case class Bad(reason: String) extends Status

  test("Type class instances derived using Shapeless 3") {

    import com.xebia.functional.typeclassderivation.shapeless3.Show

    case class Bar(x: Int, y: String, z: Boolean) derives Show

    assertEquals(Show[Bar].show(Bar(1, "s", true)), "Bar(x = 1, y = s, z = true)")
    assertEquals(Show[Foo].show(Foo(1, "s", true)), "Foo(x = 1, y = s, z = true)")
    assertEquals(Show[ColorEnum].show(ColorEnum.Blue), "Blue()")
    assertEquals(Show[Color].show(Color.Blue), "Blue()")
    assertEquals(
      Show[StatusEnum].show(StatusEnum.Good(100)),
      "Good(score = 100)"
    )
    assertEquals(
      Show[StatusEnum].show(StatusEnum.Bad("fail")),
      "Bad(reason = fail)"
    )
    assertEquals(
      Show[Status].show(Status.Good(100)),
      "Good(score = 100)"
    )
    assertEquals(
      Show[Status].show(Status.Bad("fail")),
      "Bad(reason = fail)"
    )

  }

  test("Type class instances derived using only Scala 3") {

    import com.xebia.functional.typeclassderivation.scala3.Show

    case class Bar(x: Int, y: String, z: Boolean) derives scala3.Show

    assertEquals(Show[Bar].show(Bar(1, "s", true)), "Bar(x = 1, y = s, z = true)")
    assertEquals(Show[Foo].show(Foo(1, "s", true)), "Foo(x = 1, y = s, z = true)")
    assertEquals(Show[ColorEnum].show(ColorEnum.Blue), "Blue()")
    assertEquals(Show[Color].show(Color.Blue), "Blue()")
    assertEquals(
      Show[StatusEnum].show(StatusEnum.Good(100)),
      "Good(score = 100)"
    )
    assertEquals(
      Show[StatusEnum].show(StatusEnum.Bad("fail")),
      "Bad(reason = fail)"
    )
    assertEquals(
      Show[Status].show(Status.Good(100)),
      "Good(score = 100)"
    )
    assertEquals(
      Show[Status].show(Status.Bad("fail")),
      "Bad(reason = fail)"
    )

  }

}
