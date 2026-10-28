package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.automaton.Branch.*

class BranchSuite extends munit.FunSuite:

  // `(?!1)2|3`, whose look-ahead tests the leaf `1`.
  val branch: Branch[Int] = Choice(Vector(LookAhead(true, Leaf(1), Leaf(2)), Leaf(3)))

  test("Branch#leaves"):
    assertEquals(branch.leaves, Seq(1, 2, 3))
    assertEquals(Succeed.leaves, Seq.empty)

  test("Branch#succeeds"):
    assertEquals(branch.succeeds(Set(2)), true)
    assertEquals(branch.succeeds(Set(1, 2)), false)
    assertEquals(branch.succeeds(Set(1, 3)), true)
    assertEquals(Succeed.succeeds(_ => false), true)
    assertEquals(Fail.succeeds(_ => true), false)

  test("Branch#explore"):
    assertEquals(branch.explore(Set(2)), Seq(1, 2))
    assertEquals(branch.explore(Set(1, 2)), Seq(1, 3))
    assertEquals(branch.explore(Set.empty), Seq(1, 2, 3))
    assertEquals(Choice(Vector(Succeed, Leaf(1))).explore(_ => false), Seq.empty)

  test("Branch#map"):
    assertEquals(branch.map(_ * 2), Choice(Vector(LookAhead(true, Leaf(2), Leaf(4)), Leaf(6))))

  test("Branch#flatMap"):
    assertEquals(branch.flatMap(q => if q == 1 then Fail else Leaf(q)), Choice(Vector(Leaf(2), Leaf(3))))
    assertEquals(branch.flatMap(q => if q == 1 then Succeed else Leaf(q)), Leaf(3))

  test("Branch.choice"):
    assertEquals(Branch.choice(Seq(Leaf(1))), Leaf(1))
    assertEquals(
      Branch.choice(Seq(Choice(Vector(Leaf(1), Leaf(2))), Leaf(3))),
      Choice(Vector(Leaf(1), Leaf(2), Leaf(3)))
    )
    assertEquals(Branch.choice(Seq(Leaf(1), Succeed, Leaf(2))), Choice(Vector(Leaf(1), Succeed)))
    assertEquals(Branch.choice(Seq.empty[Branch[Int]]), Fail)

  test("Branch.lookAhead"):
    assertEquals(Branch.lookAhead(false, Fail, Leaf(1)), Fail)
    assertEquals(Branch.lookAhead(true, Fail, Leaf(1)), Leaf(1))
    assertEquals(Branch.lookAhead(false, Succeed, Leaf(1)), Leaf(1))
    assertEquals(Branch.lookAhead(true, Succeed, Leaf(1)), Fail)
    assertEquals(Branch.lookAhead(false, Leaf(1), Leaf(2)), LookAhead(false, Leaf(1), Leaf(2)))
