package codes.quine.labs.resyntax.charset

class CodecSuite extends munit.FunSuite:
  test("Codec.decode"):
    assertEquals(Codec.decode("").toSeq, Seq.empty)
    assertEquals(Codec.decode(" !#").toSeq, Seq(0, 1, 2))
    assertEquals(Codec.decode("O").toSeq, Seq(46))
    assertEquals(Codec.decode("Q!").toSeq, Seq(47))
    assertEquals(Codec.decode("Q! !").toSeq, Seq(47, 0, 1))

  test("Codec.unzigzag"):
    assertEquals(Seq(0, 1, 2, 3, 4).map(Codec.unzigzag), Seq(0, -1, 1, -2, 2))

  test("Codec.intervals"):
    assertEquals(Codec.intervals("!#!$"), CharSet.fromIntervals(Seq((1, 3), (4, 7))))

  test("Codec.strings"):
    assertEquals(Codec.strings(" #%'"), Seq(UString(2, 3)))
    assertEquals(Codec.strings(" #%'!!#"), Seq(UString(2, 3), UString(2, 4)))

  test("Codec.overrides"):
    assertEquals(Codec.overrides("$!%!#&'"), Seq((3, UString(4)), (4, UString(5, 6))))

  test("Codec.names"):
    assertEquals(Codec.names("White_Space,WSpace,space"), (IndexedSeq("White_Space", "WSpace", "space"), "WSpace"))
    assertEquals(Codec.names("Dash,Dash"), (IndexedSeq("Dash"), "Dash"))
    assertEquals(Codec.names("alnum,"), (IndexedSeq("alnum"), "alnum"))
