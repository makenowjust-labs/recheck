package codes.quine.labs.recheck.common

class AttackStringCharacteristicsSuite extends munit.FunSuite:

  test("AttackStringCharacteristics#toString"):
    assertEquals(AttackStringCharacteristics.None.toString, "none")
    assertEquals(AttackStringCharacteristics.Latin1.toString, "latin1")
    assertEquals(AttackStringCharacteristics.CharClass.toString, "char-class")
    assertEquals(AttackStringCharacteristics.Alphabet.toString, "alphabet")
