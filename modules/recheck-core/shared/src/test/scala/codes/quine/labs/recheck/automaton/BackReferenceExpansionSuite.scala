package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.dialect.Dialect

class BackReferenceExpansionSuite extends munit.FunSuite:

  given ctx: Context = Context()

  test("BackReferenceExpansion.groups"):
    def groups(source: String, flags: String = "", dialect: Dialect = Dialect.JavaScript): Option[Set[Int]] =
      BackReferenceExpansion.groups(ReDoS.parse(source, flags, dialect), 100)

    assertEquals(groups("^(a|b)\\1$"), Some(Set(1)))
    assertEquals(groups("^([\\q{ab}c])\\1$", "v"), Some(Set(1)))
    assertEquals(groups("^((a)b)\\1\\2$"), Some(Set(1, 2)))
    assertEquals(groups("^(a{2}b{0,2})\\1$"), Some(Set(1)))
    // The groups match too many strings, have a construct that the expansion does not handle, or an atomic group holds
    // them.
    assertEquals(groups("^([a-z]{2})\\1$"), None)
    assertEquals(groups("^(a+)\\1$"), None)
    assertEquals(groups("^(a(?=b))\\1$"), None)
    assertEquals(groups("^(?>(a))\\1$", "", Dialect.PCRE), None)

  test("BackReferenceExpansion.refinements"):
    val pattern = ReDoS.parse("^([\\q{ab}c])\\1$", "v", Dialect.JavaScript)
    val sets = BackReferenceExpansion.refinements(pattern, Set(1))
    assertEquals(sets.toSet, Set(CharSet.single('a'), CharSet.single('b'), CharSet.single('c')))
