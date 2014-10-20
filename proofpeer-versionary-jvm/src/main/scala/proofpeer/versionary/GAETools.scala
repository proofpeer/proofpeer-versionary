package proofpeer.versionary

import com.google.appengine.api.datastore._

object GAETools {

  def currentTime() : Long = System.currentTimeMillis()

  val ONE_HOUR = 60L * 60L * 1000L
  val ONE_DAY = 24L * ONE_HOUR
  val ONE_WEEK = 7L * ONE_DAY  

  import Query._

  case class PropertyName(name : String)
  
  val KEY = PropertyName(Entity.KEY_RESERVED_PROPERTY)

  def $(name : String) = PropertyName(name)

  def equal(p : PropertyName, x : Any) : Filter = 
    new FilterPredicate(p.name, FilterOperator.EQUAL, x)

  def equal(x : Any, p : PropertyName) : Filter = equal(p, x)

  def notequal(p : PropertyName, x : Any) : Filter = 
    new FilterPredicate(p.name, FilterOperator.NOT_EQUAL, x)

  def notequal(x : Any, p : PropertyName) : Filter = notequal(p, x)

  def less(p : PropertyName, x : Any) : Filter =
    new FilterPredicate(p.name, FilterOperator.LESS_THAN, x)

  def less(x : Any, p : PropertyName) : Filter = greater(p, x)

  def leq(p : PropertyName, x : Any) : Filter =
    new FilterPredicate(p.name, FilterOperator.LESS_THAN_OR_EQUAL, x)

  def leq(x : Any, p : PropertyName) : Filter = geq(p, x)

  def greater(p : PropertyName, x : Any) : Filter =
    new FilterPredicate(p.name, FilterOperator.GREATER_THAN, x)

  def greater(x : Any, p : PropertyName) : Filter = less(p, x)

  def geq(p : PropertyName, x : Any) : Filter =
    new FilterPredicate(p.name, FilterOperator.GREATER_THAN_OR_EQUAL, x)

  def geq(x : Any, p : PropertyName) : Filter = leq(p, x)

  def in(p : PropertyName, x : Any*) = {
    new FilterPredicate(p.name, FilterOperator.IN, List(x : _*))
  }

  def and(p : Filter, q : Filter*) : Filter = {
    var u = p
    for (v <- q) u = CompositeFilterOperator.and(u, v)
    u
  }

  def or(p : Filter, q : Filter*) : Filter = {
    var u = p
    for (v <- q) u = CompositeFilterOperator.or(u, v)
    u
  }

  def query(kind : String, filter : Filter) : Query = {
    new Query(kind).setFilter(filter)
  }

  def query(ancestor : Key, kind : String, filter : Filter) : Query = {
    query(kind, filter).setAncestor(ancestor)
  }

  def atomic(datastore : DatastoreService)(operation :  => Unit) {
    val transaction = datastore.beginTransaction()
    var rollback : Boolean = true
    try {
      operation
      rollback = false
    } catch {
      case x : scala.runtime.NonLocalReturnControl[_] =>
        rollback = false
        throw x
    } finally {
      if (transaction.isActive) {
        if (rollback)
          transaction.rollback()
        else 
          transaction.commit()
      }
    }
  }

  def get(datastore : DatastoreService, key : Key) : Option[Entity] = {
    try {
      Some(datastore.get(key))
    } catch {
      case e : EntityNotFoundException => None
    }
  }

  def fetch(datastore : DatastoreService, query : Query, maxNum : Int) : List[Entity] = {
    import scala.collection.JavaConverters._    
    List[Entity]() ++ datastore.prepare(query).asList(FetchOptions.Builder.withLimit(maxNum)).asScala
  }

}
