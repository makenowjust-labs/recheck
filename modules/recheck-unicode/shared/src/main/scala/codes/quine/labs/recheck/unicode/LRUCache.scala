package codes.quine.labs.recheck.unicode

import codes.quine.labs.recheck.unicode.LRUCache.LRUMap

/** LRUCache is a LRU (last recent used) cache. */
private[unicode] final class LRUCache[K, V](val maxSize: Int):
  private val map: LRUMap[K, V] = new LRUMap(maxSize)

  /** Store the value with the given key. */
  def put(k: K, v: V): Unit = synchronized(map.put(k, v))

  /** Obtains the value from the key. */
  def get(k: K): Option[V] =
    synchronized(if map.containsKey(k) then Some(map.get(k)) else None)

  /** Obtains the value if the key exists, or computes the new value and stores it. */
  def getOrElseUpdate(k: K)(v: => V): V = synchronized(map.computeIfAbsent(k, _ => v))

object LRUCache:

  /** LRUMap is a derivation of j.u.LinkedHashMap for LRU caching. */
  private class LRUMap[K, V](maxSize: Int) extends java.util.LinkedHashMap[K, V](maxSize, 0.75f, true):
    override def removeEldestEntry(eldest: java.util.Map.Entry[K, V]): Boolean = size > maxSize
