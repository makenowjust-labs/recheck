package codes.quine.labs.resyntax.dialect

class DotNetCharClassSuite extends munit.FunSuite:

  test("DotNetCharClass.Word"):
    // U+0301 is `Mn`, U+0903 is `Mc`, U+2160 is `Nl`, and U+203F is `Pc`.
    for c <- Seq[Int]('a', '_', '0', 0x0301, 0x203f) do assert(DotNetCharClass.Word.contains(c), c)
    for c <- Seq[Int]('-', 0x0903, 0x2160, 0x200c, 0x200d) do assert(!DotNetCharClass.Word.contains(c), c)

  test("DotNetCharClass.BoundaryWord"):
    assertEquals(DotNetCharClass.BoundaryWord.diff(DotNetCharClass.Word).iterator.toSeq, Seq(0x200c, 0x200d))
