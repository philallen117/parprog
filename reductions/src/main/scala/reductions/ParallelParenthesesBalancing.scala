package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var i: Int = 0; var depth: Int = 0
    while (depth >= 0 & i < chars.length) {
      val c = chars(i)
      if (c == '(') depth += 1
      else if (c == ')') depth -= 1
      i += 1
    }
    depth == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    case class Brackets(finalDepth: Int, hwm: Int) // hwm <= 0

    def traverse(idx: Int, until: Int, d: Int, h: Int): Brackets = {
      var i: Int = idx; var depth: Int = d; var hwm: Int = h
      while (i < until) {
        val c = chars(i)
        if (c == '(') depth += 1
        else if (c == ')') { depth -= 1; hwm = Math.min(hwm, depth) }
        i += 1
      }
      Brackets(depth, hwm)
    }

    def reduce(from: Int, until: Int): Brackets = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = (from + until) / 2
        val (reduceLeft, reduceRight) = common.parallel(reduce(from,mid), reduce(mid,until))
        Brackets(
          reduceLeft.finalDepth + reduceRight.finalDepth,
          Math.min(reduceLeft.hwm, reduceLeft.finalDepth + reduceRight.hwm)
        )
      }
    }

    reduce(0, chars.length) == Brackets(0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
