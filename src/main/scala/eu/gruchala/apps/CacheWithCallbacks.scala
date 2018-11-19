package eu.gruchala.apps

import java.util.concurrent.ConcurrentHashMap

import scala.concurrent.{ExecutionContext, Future}

trait Storage[T] {
  def get(key: String) : Option[T]
  def set(key: String, value: T) : Unit
  def clear(key: String) : Option[T]
}

object DbStorage {
  object Limits {
    val MinPersistenceDelayMs : Long = 20l
    val MaxPersistenceDelayMs : Long = 80l
    val MinClearDelayMs : Long = 10l
    val MaxClearDelayMs : Long = 50l

    def persistenceDelay() : Long = delay(MinPersistenceDelayMs, MaxPersistenceDelayMs)

    def clearanceDelay() : Long = delay(MinClearDelayMs, MaxClearDelayMs)

    private def delay(min : Long, max : Long) : Long = {
      min + math.round(math.random * (max - min))
    }
  }
}

class DbStorage[T] extends Storage[T] {
  private val db = new ConcurrentHashMap[String,T]()

  // average runtime ~1ms
  def get(key: String) : Option[T] = {
    Option(db.get(key))
  }
  // average runtime ~50ms
  def set(key: String, value: T) : Unit = {
    Thread.sleep(DbStorage.Limits.persistenceDelay())
    db.put(key, value)
  }
  // average runtime ~30ms
  def clear(key: String) : Option[T] = {
    Thread.sleep(DbStorage.Limits.clearanceDelay())
    val existing = Option(db.get(key))
    db.remove(key)
    existing
  }
}

class InMemoryCache[T](db: DbStorage[T])(implicit val ex: ExecutionContext) extends Storage[T] {

  private val inMemoryCache = new ConcurrentHashMap[String, T]()
  private val callbacks = new ConcurrentHashMap[String, Future[Unit]]()

  override def get(key: String): Option[T] =
    Option(inMemoryCache.get(key)).orElse {
      db.get(key)
    }

  override def set(key: String, value: T): Unit = {
    inMemoryCache.put(key, value)
    val chain = Option(callbacks.get(key)) match {
      case None => Future(db.set(key, value))
      case Some(f) => f.map(_ => db.set(key, value))
    }
    callbacks.put(key, chain)
  }

  override def clear(key: String): Option[T] = {
    Future(db.clear(key))
    Option(inMemoryCache.remove(key))
  }
}
