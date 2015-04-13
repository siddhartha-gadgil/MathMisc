package scripts

object PermSurf{
  def skip(k: Int)(n : Int) = if (n< k) n else n+1

  def withHead(k: Int)(l: List[Int]) = k :: (l map (skip(k)(_)))

  /**
   * permutations on 0, 1, ,..., n-1 as an iterator of lists (which are also functions)
   */
  def perms(n: Int): Iterator[List[Int]] = {
    if (n == 1) List(List(0)).toIterator else
      for (p<- perms(n-1); i <- 0 to n-1) yield (withHead(i)(p))
  }

  /**
   * Permutation as a function.
   */
  type Perm = Int => Int

  /**
   * check recursively equality for all entries of a permutation
   *
   *  @param n check up to (n-1) and combine with previously computed values
   *  @param accum the result of checking equality for higher relevant indices.
   */
  @annotation.tailrec def eql(n: Int, a : Perm, b: Perm,
      accum: Boolean = true) : Boolean = {
    if (n < 1) accum // no more case to check, give result so far.
    else {
      if (accum) eql(n-1, a, b, a(n-1)== b(n-1)) else false
    }
  }


  /**
   * inverse of a permutation given as a list.
   */
  def inv(l: List[Int]): Perm = (n: Int) => l.indexOf(n)

  /**
   * S_n -> S_{n+1} fixing the last index.
   */
  def pad(l: List[Int]) = l :+ l.length

  /**
   * Conjugate the  standard cycle k \mapsto (k + 1) % n by the list l.
   */
  def mkCycle(l: List[Int]): Perm = (n: Int) => l((inv(l)(n) + 1) % l.length)

  /**
   * all cycles of length n, obtained from permuations of size n-1.
   */
  def allCycles(n: Int) = for (p <- perms(n-1)) yield mkCycle(pad(p))

  /**
   * checking parity condition, similar to equality check
   */
  def paritycheck(n: Int, p: Perm, accum: Boolean = true) : Boolean = {
    if (n < 3) accum else {
      if (accum) paritycheck(n-1, p,
          p(n - 1) % 2 == p ((n - 1) % 2)) else false
    }
  }

  val mu = Map(1 -> 7, 7 -> 1, 2 -> 8, 8 -> 2, 3 -> 9, 9 -> 3,
      4 -> 10, 10 -> 4, 5 -> 11, 11 -> 5, 6 -> 12, 12 -> 6)

  val tau = Map(1 -> 3, 3 -> 5, 5 -> 1,
      2 ->4, 4 ->6, 6 -> 2,
      11 -> 9, 9 -> 7, 7 -> 11,
      12 -> 10, 10 -> 8, 8 -> 12)

  def condition(sigma: Perm) = paritycheck(12, sigma) && eql(12,
      (i: Int) => sigma(mu(sigma(i))), tau)

  val result = allCycles(12) filter condition

  def main(args: Array[String]): Unit ={
    println(result.toStream.headOption)
  }
}
