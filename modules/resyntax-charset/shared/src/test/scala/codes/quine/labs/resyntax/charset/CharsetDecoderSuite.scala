package codes.quine.labs.resyntax.charset

class CharsetDecoderSuite extends munit.FunSuite:
  test("CharsetDecoder.decode"):
    assertEquals(CharsetDecoder.decode("").toSeq, Seq.empty)
    assertEquals(CharsetDecoder.decode(" !#").toSeq, Seq(0, 1, 2))
    assertEquals(CharsetDecoder.decode("O").toSeq, Seq(46))
    assertEquals(CharsetDecoder.decode("Q!").toSeq, Seq(47))
    assertEquals(CharsetDecoder.decode("Q! !").toSeq, Seq(47, 0, 1))

  test("CharsetDecoder.unzigzag"):
    assertEquals(Seq(0, 1, 2, 3, 4).map(CharsetDecoder.unzigzag), Seq(0, -1, 1, -2, 2))

  test("CharsetDecoder.decodeIntervals"):
    assertEquals(CharsetDecoder.decodeIntervals("!#!$"), CharSet.range(1, 2).union(CharSet.range(4, 6)))

  test("CharsetDecoder.decodeStrings"):
    assertEquals(CharsetDecoder.decodeStrings(" #%'"), Seq(UString(2, 3)))
    assertEquals(CharsetDecoder.decodeStrings(" #%'!!#"), Seq(UString(2, 3), UString(2, 4)))

  test("CharsetDecoder.decodeOverrides"):
    assertEquals(CharsetDecoder.decodeOverrides("$!%!#&'"), Seq((3, UString(4)), (4, UString(5, 6))))

  test("CharsetDecoder.decodeNames"):
    assertEquals(
      CharsetDecoder.decodeNames("White_Space,WSpace,space"),
      (IndexedSeq("White_Space", "WSpace", "space"), "WSpace")
    )
    assertEquals(CharsetDecoder.decodeNames("Dash,Dash"), (IndexedSeq("Dash"), "Dash"))
    assertEquals(CharsetDecoder.decodeNames("alnum,"), (IndexedSeq("alnum"), "alnum"))
