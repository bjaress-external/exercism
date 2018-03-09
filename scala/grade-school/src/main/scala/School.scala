import scala.collection.immutable.SortedMap

class School {
  type DB = Map[Int, Seq[String]]

  private var students: DB = SortedMap()

  def add(name: String, g: Int) = {
    students = students.updated(g, grade(g) :+ name)
  }

  def grade(g: Int) = students.getOrElse(g, Seq())

  def db: DB = students
  def sorted: DB = students mapValues (_.sorted)
}
