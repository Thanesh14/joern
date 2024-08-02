package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.parser
import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class ArrayParserTests extends RubyParserFixture with Matchers {
  "array structures" in {
    test("[]")
    test("%w[]")
    test("%i[]")
    test("%w[x y z]")
    test("%w(x y z)")
    test("%w{x y z}")
    test("%w<x\\ y>")
    test("%w-x y z-")
    test(
      """%w(
           | bob
           | cod
           | dod
           |)""".stripMargin,
      """%w(bob
        |cod
        |dod)""".stripMargin
    )
    test("%W(x#{1})")
    test(
      """%W[
           | x#{0}
           |]""".stripMargin,
      "%W[x#{0}]"
    )
    test("%W()")
    test("%i<x y>")
    test("%i{x\\ y}")
    test("%i[x [y]]")
    test("%i[x [y]]")
    test(
      """%i(
           |x y
           |z
           |)""".stripMargin,
      """%i(x
        |y
        |z)""".stripMargin
    )
  }

  "fixme" ignore {
    test("%I{}")         // Unknown in `RubyNodeCreator`
    test("%I(x#{0} x1)") // Interpolations are weird
  }
}
