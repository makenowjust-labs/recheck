package codes.quine.labo.re
package unicode

import minitest.SimpleTestSuite

import data.UChar

object CaseMapSuite extends SimpleTestSuite {
  test("CaseMap.Upper") {
    // Checks there is [a-z] to [A-Z] mapping.
    assert(CaseMap.Upper.find(_.offset == -32).exists(_.domain.intervals.contains((UChar(0x61), UChar(0x7b)))))
  }

  test("CaseMap.Fold") {
    // Checks there is [A-Z] to [a-z] mapping.
    assert(CaseMap.Fold.find(_.offset == 32).exists(_.domain.intervals.contains(UChar(0x41), UChar(0x5b))))
  }
}
