package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.automaton.Branch.*

class BranchSuite extends munit.FunSuite:

  // `(?!1)2|3`, whose look-ahead tests the leaf `1`.
  val branch: Branch[Int] = Choice(Vector(LookAhead(true, Leaf(1), Leaf(2)), Leaf(3)))

  // A choice in an atomic group, which commits to `2` if `1` succeeds, or else to `4`.
  val atomic: Branch[Int] = AtomicChoice(Vector((Leaf(1), Leaf(2))), Leaf(4))

  test("Branch#leaves"):
    assertEquals(branch.leaves, Seq(1, 2, 3))
    assertEquals(Succeed.leaves, Seq.empty)
    assertEquals(atomic.leaves, Seq(2, 4))

  test("Branch#allLeaves"):
    assertEquals(branch.allLeaves, Seq(1, 2, 3))
    assertEquals(atomic.allLeaves, Seq(1, 2, 4))

  test("Branch#succeeds"):
    assertEquals(branch.succeeds(Set(2)), true)
    assertEquals(branch.succeeds(Set(1, 2)), false)
    assertEquals(branch.succeeds(Set(1, 3)), true)
    assertEquals(Succeed.succeeds(_ => false), true)
    assertEquals(Fail.succeeds(_ => true), false)
    assertEquals(atomic.succeeds(Set(1, 4)), false)
    assertEquals(atomic.succeeds(Set(3, 4)), true)

  test("Branch#explore"):
    assertEquals(branch.explore(Set(2)), Seq(1, 2))
    assertEquals(branch.explore(Set(1, 2)), Seq(1, 3))
    assertEquals(branch.explore(Set.empty), Seq(1, 2, 3))
    assertEquals(Choice(Vector(Succeed, Leaf(1))).explore(_ => false), Seq.empty)
    // An atomic choice explores the alternatives up to the one that it commits to.
    assertEquals(atomic.explore(Set(1)), Seq(2))
    assertEquals(atomic.explore(Set(3)), Seq(2, 4))
    assertEquals(atomic.explore(Set.empty), Seq(2, 4))

  test("Branch#minExplored"):
    assertEquals(Leaf(1).minExplored, 1)
    assertEquals(Succeed.minExplored, 0)
    assertEquals(Fail.minExplored, 0)
    assertEquals(Choice(Vector(Leaf(1), Leaf(2))).minExplored, 1)
    assertEquals(Choice(Vector(Succeed, Leaf(1))).minExplored, 0)
    assertEquals(branch.minExplored, 1)
    assertEquals(atomic.minExplored, 1)
    // It is not over the leaves that `explore` lists, whichever leaves succeed.
    for b <- Seq(branch, atomic); succeeding <- Set(1, 2, 3, 4).subsets() do
      assert(b.minExplored <= b.explore(succeeding).size, s"$b with $succeeding")

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

  test("Branch.atomicChoice"):
    assertEquals(Branch.atomicChoice(Seq((Fail, Fail), (Succeed, Leaf(2)), (Leaf(3), Leaf(4))), Leaf(5)), Leaf(2))
    assertEquals(Branch.atomicChoice(Seq((Fail, Fail)), Leaf(5)), Leaf(5))
    assertEquals(
      Branch.atomicChoice(Seq((Fail, Leaf(1)), (Succeed, Leaf(2))), Leaf(5)),
      AtomicChoice(Vector((Fail, Leaf(1))), Leaf(2))
    )
    assertEquals(
      Branch.atomicChoice(Seq((Leaf(1), Leaf(2)), (Succeed, Leaf(3)), (Leaf(4), Leaf(5))), Leaf(6)),
      AtomicChoice(Vector((Leaf(1), Leaf(2))), Leaf(3))
    )
    assertEquals(
      atomic.flatMap(q => if q == 1 then Fail else Leaf(q)),
      AtomicChoice(Vector((Fail, Leaf(2))), Leaf(4))
    )
