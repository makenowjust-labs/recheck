package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.automaton.LookBehindProduct.ProductState
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.resyntax.dialect.Dialect

class LookBehindProductSuite extends munit.FunSuite:

  given ctx: Context = Context()

  /** Builds the ordered NFA of the source and its NFA with look-ahead. */
  def build(source: String): (OrderedNFA[Int, Int], NFAwLA[Int, Int, Set[Int]]) =
    val ir = ReDoS.parse(source, "", Dialect.JavaScript)
    val epsNFA = EpsNFABuilder.build(ir, Int.MaxValue, Parameters.DefaultMaxBackReferenceStrings)
    val nfa = epsNFA.toOrderedNFA(false, Int.MaxValue, Int.MaxValue).rename.mapAlphabet(_.head)
    (nfa, nfa.toNFAwLA(nfa.toLookAheadDFA(Int.MaxValue), identity, false, Int.MaxValue))

  test("LookBehindProduct.build"):
    val (nfa, nfaWLA) = build("^a*(?<=a)a*$")
    val product = LookBehindProduct.build(nfaWLA, nfa.lookBehind.get, Int.MaxValue)
    assert(product.acceptSet.nonEmpty)
    assert(product.acceptSet.forall(r => r._1 == ProductState.After() || r._1 == ProductState.AfterEnd()))
    assert(product.initSet.toSet.forall(_._1.isInstanceOf[ProductState.Before[?]]))
    // A path of the inner expression starts on `a` before the look-behind assertion.
    assert(product.stateSet.exists(_._1.isInstanceOf[ProductState.Inner[?]]))

  test("LookBehindProduct.build: maxNFASize"):
    val (nfa, nfaWLA) = build("^a*(?<=a)a*$")
    interceptMessage[UnsupportedException]("look-behind product is too large"):
      LookBehindProduct.build(nfaWLA, nfa.lookBehind.get, 1)
