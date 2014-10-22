package proofpeer.versionary

object TicTacToe {

  val EMPTY = 0
  val CIRCLE = 1
  val CROSS = 2

  type Field = Vector[Int]
  type Permutation = Vector[Int]

  val emptyField : Field = Vector(0, 0, 0, 0, 0, 0, 0, 0, 0)

  val lines : Vector[Vector[Int]] = Vector(
    Vector(0, 1, 2),
    Vector(3, 4, 5),
    Vector(6, 7, 8),
    Vector(0, 3, 6),
    Vector(1, 4, 7),
    Vector(2, 5, 8),
    Vector(0, 4, 8),
    Vector(2, 4, 6)
  )

  var permutations : Set[Permutation] = Set(
    Vector(0, 1, 2, 3, 4, 5, 6, 7, 8),
    Vector(2, 5, 8, 1, 4, 7, 0, 3, 6),
    Vector(8, 7, 6, 5, 4, 3, 2, 1, 0),
    Vector(6, 3, 0, 7, 4, 1, 8, 5, 2),
    Vector(0, 3, 6, 1, 4, 7, 2, 5, 8),
    Vector(2, 1, 0, 5, 4, 3, 8, 7, 6)
  )

  def permApply(p : Permutation, x : Vector[Int]) = {
    p.map(i => x(i))
  }

  def permMul(perm1 : Vector[Int], perm2 : Vector[Int]) : Permutation = {
    permApply(perm2, perm1)
  }

  def completePermutations() {
    var s = -1
    do {
      s = permutations.size
      for (p1 <- permutations) {
        for (p2 <- permutations) {
          permutations += permMul(p1, p2)
        }
      }
    } while (permutations.size != s)
  }
  
  completePermutations()

  def countLines(field : Field, t : Int) : Int = {
    var x : Int = 0
    for (line <- lines) {
      if (field(line(0)) == t && field(line(1)) == t && field(line(2)) == t) x += 1
    }
    x
  }

  def countAll(field : Field, t : Int) : Int = {
    var x : Int = 0
    for (i <- 0 until 9) if (field(i) == t) x += 1
    x
  }

  def isValidField(field : Field) : Boolean = {
    val delta = countAll(field, CIRCLE) - countAll(field, CROSS)
    val circlewins = countLines(field, CIRCLE) > 0
    val crosswins = countLines(field, CROSS) > 0
    val onewinner = !(circlewins && crosswins)
    (delta >= 0 && delta <= 1 && onewinner)
  }

  def genAllFields(len : Int) : List[Field] = {
    if (len == 0) return List(Vector())
    val fields = genAllFields(len - 1)
    var newfields : List[Field] = List()
    for (f <- fields) {
      newfields = (f ++ Vector(EMPTY)) :: newfields
      newfields = (f ++ Vector(CIRCLE)) :: newfields
      newfields = (f ++ Vector(CROSS)) :: newfields
    }
    newfields
  } 

  val allFields = genAllFields(9)
  val allValidFields = allFields.filter(isValidField _)

  var validFields : Set[Field] = Set()

  def addField(field : Field) : Boolean = {
    if (validFields.contains(field)) false
    else {
      for (permutation <- permutations) {
        validFields += permApply(permutation, field)
      }
      true
    }
  }

  def main(args : Array[String]) {
    println("number of valid fields: " + allValidFields.size)
    var modulo = 0
    for (field <- allValidFields) {
      if (addField(field)) modulo += 1
    }
    println("number of permutations: " + permutations.size)
    println("number of valid fields modulo symmetry: " + modulo)
  }

}