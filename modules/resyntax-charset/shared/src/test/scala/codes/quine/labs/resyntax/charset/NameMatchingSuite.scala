package codes.quine.labs.resyntax.charset

class NameMatchingSuite extends munit.FunSuite:

  test("NameMatching.loose (UAX44-LM3)"):
    assertEquals(NameMatching.loose("Is_Line-Break Foo"), "linebreakfoo")
    assertEquals(NameMatching.loose("ISO_Comment"), "ocomment")
