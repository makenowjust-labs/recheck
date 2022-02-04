package codes.quine.labs.recheck.unicode

class LRUCacheSuite extends munit.FunSuite {
  test("LRUCache#put") {
    val cache = new LRUCache[Int, Int](2)
    cache.put(1, 1)
    assertEquals(cache.get(1), Some(1))
    cache.put(1, 2)
    assertEquals(cache.get(1), Some(2))
    cache.put(2, 2)
    cache.put(3, 3)
    assertEquals(cache.get(1), None)
    assertEquals(cache.get(2), Some(2))
    assertEquals(cache.get(3), Some(3))
  }
}
