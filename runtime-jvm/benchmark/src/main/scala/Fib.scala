package org.unisonweb.benchmark

import org.unisonweb.Term._
import org.unisonweb.Runtime._

object Fib extends App {

  implicit class Arithmetic(a: Term) {
    def -(b: Term) = Builtin("-")(a,b)
    def +(b: Term) = Builtin("+")(a,b)
  }

  val builtins : String => Rt = {
    case s@"-" => new Arity2(Builtin(s)) with NF {
      def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
        r.unboxed = x2 - x1
      }
    }
    case s@"+" => new Arity2(Builtin(s)) with NF {
      def apply(rec: Rt, x1: D, x1b: Rt, x2: D, x2b: Rt, r: R) = {
        r.unboxed = x2 + x1
      }
    }
  }

  val N = 15.0

  val fib =
    LetRec(
      "fib" -> Lam("n")(
        If0("n", 0.0,
        If0(Var("n") - 1.0, 1.0,
        Var("fib")(Var("n") - 1.0) + Var("fib")(Var("n") - 2.0))))
    )(Var("fib")(N))

  def fibScala(n: Double): Double =
    if (n == 0.0) 0.0
    else if (n - 1.0 == 0.0) 1.0
    else fibScala(n-1.0) + fibScala(n-2.0)

  val plus = compile(builtins)(Builtin("+"))
  val minus1 = compile(builtins)(Lam("n")(Var("n") - 1.0))
  val minus2 = compile(builtins)(Lam("n")(Var("n") - 2.0))

  val manuallyCompiledFib : Rt = new Arity1(Builtin("fib-manual-compile")) {
    def bind(env: Map[Name,Rt]) = ()
    def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
      if (x1 == 0.0) r.unboxed = 0.0
      else {
        // val x12 = eval(null, minus1, x1, null, r)
        if (x1 == 1.0) r.unboxed = 1.0
        // if (r.unboxed == 0.0) r.unboxed = 1.0
        else {
          val r1 = { apply(null, x1 - 1.0, null, r); r.unboxed }
          val r2 = { apply(null, x1 - 2.0, null, r); r.unboxed }
          plus(null, r1, null, r2, null, r)
        }
      }
    }
    override def isEvaluated = true
  }

  val manuallyCompiledFib2 : Rt = new Arity1(Builtin("fib-manual-compile")) {
    def bind(env: Map[Name,Rt]) = ()
    def apply(rec: Rt, x1: D, x1b: Rt, r: R) = {
      if (x1 == 0.0) r.unboxed = 0.0
      else {
        if (x1 == 1.0) r.unboxed = 1.0
        else {
          val r1 = { rec(rec, x1 - 1.0, null, r); r.unboxed }
          val r2 = { rec(rec, x1 - 2.0, null, r); r.unboxed }
          plus(null, r1, null, r2, null, r)
        }
      }
    }
    override def isEvaluated = true
  }

  // observation - function call overhead is bad for stuff like addition
  // fib (n - 1)
  // has to pass n to decrement fn, then take result and pass that to fib
  // n has to be copied to arg
  // basically, too much copying around
  // wonder if there's some way to decrease that overhead?
  // n - 1

  case class R2(var get: Rt2)

  trait Rt2 {
    def apply(rec: Rt2, r: R2): Double
    def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double
    def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double
  }
  object Rt2 {
    case class TC(fn: Rt2, x1: D, x1b: Rt2) extends Throwable { override def fillInStackTrace = this }

    @annotation.tailrec
    def tailCallLoop(tc: TC, r: R2): Double =
      try tc.fn(tc.fn, tc.x1, tc.x1b, r)
      catch { case tc: TC => tailCallLoop(tc, r) }

    def eval(rec: Rt2, e: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double =
      try e(rec, x1, x1b, x2, x2b, r)
      catch { case tc0: TC => tailCallLoop(tc0, r) }

    def eval(rec: Rt2, e: Rt2, x1: D, x1b: Rt2, r: R2): Double =
      try e(rec, x1, x1b, r)
      catch { case tc0: TC => tailCallLoop(tc0, r) }

    def eval(rec: Rt2, e: Rt2, r: R2): Double =
      try e(rec, r)
      catch { case tc0: TC => tailCallLoop(tc0, r) }

    val x1 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double = ???
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double = { r.get = x1b; x1 } // todo - just returning x1 is a speedup
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double = { r.get = x1b; x1 }
    }
    val x2 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double = ???
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double = ???
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double = { r.get = x2b; x2 }
    }
    val rec = new Rt2 {
      def apply(rec: Rt2, r: R2): Double = { r.get = rec; 0.0 }
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double = { r.get = rec; 0.0 }
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double = { r.get = rec; 0.0 }
    }
    def if0(cond: Rt2, if0: Rt2, ifNot0: Rt2): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double =
        if (eval(rec,cond,r) == 0.0) if0(rec, r) else ifNot0(rec, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double =
        if (eval(rec, cond, x1, x1b, r) == 0.0) if0(rec, r) else ifNot0(rec, x1, x1b, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double =
        if (eval(rec, cond, x1, x1b, x2, x2b, r) == 0.0) if0(rec, r) else ifNot0(rec, x1, x1b, x2, x2b, r)
    }
    def plus(x: Rt2, y: Rt2): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double = eval(rec, x, r) + eval(rec, y, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double = eval(rec, x, x1, x1b, r) + eval(rec, y, x1, x1b, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double =
        eval(rec, x, x1, x1b, x2, x2b, r) + eval(rec, y, x1, x1b, x2, x2b, r)
    }
    def minus(x: Rt2, y: Rt2): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double = eval(rec, x, r) - eval(rec, y, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double = eval(rec, x, x1, x1b, r) - eval(rec, y, x1, x1b, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double =
        eval(rec, x, x1, x1b, x2, x2b, r) - eval(rec, y, x1, x1b, x2, x2b, r)
    }
    def num(n: Double): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double = n
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double = n
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double = n
    }
    def apRec(a: Rt2): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2) = rec(rec, eval(rec, a, r), r.get, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2) = rec(rec, eval(rec, a, x1, x1b, r), r.get, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2) = rec(rec, eval(rec, a, x1, x1b, x2, x2b, r), r.get, r)
    }
    def ap(fn: Rt2, a: Rt2) = new Rt2 {
      def apply(rec: Rt2, r: R2) = fn(fn, eval(rec, a, r), r.get, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2) = fn(fn, eval(rec, a, x1, x1b, r), r.get, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2) = fn(fn, eval(rec, a, x1, x1b, x2, x2b, r), r.get, r)
    }
    def if1(cond: Rt2, if1: Rt2, ifNot1: Rt2): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double =
        if (eval(rec, cond, r) == 1.0) if1(rec, r) else ifNot1(rec, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double =
        if (eval(rec, cond, x1, x1b, r) == 1.0) if1(rec, r) else ifNot1(rec, x1, x1b, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double =
        if (eval(rec, cond, x1, x1b, x2, x2b, r) == 1.0) if1(rec, r) else ifNot1(rec, x1, x1b, x2, x2b, r)
    }
    def decrement(x: Rt2, by: Double): Rt2 = new Rt2 {
      def apply(rec: Rt2, r: R2): Double = eval(rec, x, r) - by
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double = eval(rec, x, x1, x1b, r) - by
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double = eval(rec, x, x1, x1b, x2, x2b, r) - by
    }

    val fib = new Rt2 {
      val body =
        if0(x1, num(0.0),
                if1(x1,
                    num(1.0),
                    plus(apRec(decrement(x1, 1.0)), apRec(decrement(x1, 2.0)))))

      def apply(rec: Rt2, r: R2) = ???
      def apply(rec: Rt2, x1: D, x1b: Rt2, r: R2): Double = body(rec, x1, x1b, r)
      def apply(rec: Rt2, x1: D, x1b: Rt2, x2: D, x2b: Rt2, r: R2): Double = ???
    }
    val fibN = ap(fib, num(N))
  }

  class R3(var unboxed: D = 0.0, var boxed: Rt3 = null)

  trait Rt3 {
    def apply(): R3
    def apply(x1: D, x1b: Rt3): R3
    def apply(x1: D, x1b: Rt3, x2: D, x2b: Rt3): R3
  }
  object Rt3 {
    val x1: Rt3 = new Rt3 {
      def apply(): R3 = ???
      def apply(x1: D, x1b: Rt3): R3 = new R3(x1, x1b)
      def apply(x1: D, x1b: Rt3, x2: D, x2b: Rt3): R3 = new R3(x1, x1b)
    }

    def num(n: Double): Rt3 = new Rt3 {
      def apply(): R3 = new R3(unboxed = n)
      def apply(x1: D, x1b: Rt3): R3 = new R3(unboxed = n)
      def apply(x1: D, x1b: Rt3, x2: D, x2b: Rt3): R3 = new R3(unboxed = n)
    }

    def if0(cond: Rt3, if0: Rt3, ifNot0: Rt3): Rt3 = new Rt3 {
      def apply(): R3 =
        if (cond().unboxed == 0.0) if0() else ifNot0()
      def apply(x1: D, x1b: Rt3): R3 =
        if (cond(x1,x1b).unboxed == 0.0) if0(x1,x1b) else ifNot0(x1,x1b)
      def apply(x1: D, x1b: Rt3, x2: D, x2b: Rt3): R3 =
        if (cond(x1, x1b, x2, x2b).unboxed == 0.0) if0(x1,x1b,x2,x2b) else ifNot0(x1,x1b,x2,x2b)
    }

    def if1(cond: Rt3, if1: Rt3, ifNot1: Rt3): Rt3 = new Rt3 {
      def apply(): R3 =
        if (cond().unboxed == 1.0) if1() else ifNot1()
      def apply(x1: D, x1b: Rt3): R3 =
        if (cond(x1,x1b).unboxed == 1.0) if1(x1,x1b) else ifNot1(x1,x1b)
      def apply(x1: D, x1b: Rt3, x2: D, x2b: Rt3): R3 =
        if (cond(x1, x1b, x2, x2b).unboxed == 1.0) if1(x1,x1b,x2,x2b) else ifNot1(x1,x1b,x2,x2b)
    }

    def decrement(v: Rt3, by: Double): Rt3 = new Rt3 {
      def apply(): R3 = new R3(unboxed = v().unboxed - by)
      def apply(x1: D, x1b: Rt3): R3 = new R3(unboxed = v(x1,x1b).unboxed - by)
      def apply(x1: D, x1b: Rt3, x2: D, x2b: Rt3): R3 = new R3(unboxed = v(x1,x1b,x2,x2b).unboxed - by)
    }

    def plus(x: Rt3, y: Rt3): Rt3 = new Rt3 {
      def apply(): R3 =
        new R3(unboxed = x().unboxed + y().unboxed)
      def apply(x1: D, x1b: Rt3): R3 =
        new R3(unboxed = x(x1,x1b).unboxed + y(x1,x1b).unboxed)
      def apply(x1: D, x1b: Rt3, x2: D, x2b: Rt3): R3 =
        new R3(unboxed = x(x1,x1b,x2,x2b).unboxed + y(x1,x1b,x2,x2b).unboxed)
    }

    // assumes f is already evaluated
    def ap(f: Rt3, arg: Rt3): Rt3 = new Rt3 {
      def apply(): R3 = {
        val r = arg()
        f(r.unboxed, r.boxed)
      }
      def apply(x1: D, x1b: Rt3): R3 = {
        val r = arg(x1, x1b)
        f(r.unboxed, r.boxed)
      }
      def apply(x1: D, x1b: Rt3, x2: D, x2b: Rt3): R3 = {
        val r = arg(x1, x1b, x2, x2b)
        f(r.unboxed, r.boxed)
      }
    }

    def apDyn(f: () => Rt3, arg: Rt3): Rt3 = new Rt3 {
      def apply(): R3 = {
        val r = arg()
        f()(r.unboxed, r.boxed)
      }
      def apply(x1: D, x1b: Rt3): R3 = {
        val r = arg(x1, x1b)
        f()(r.unboxed, r.boxed)
      }
      def apply(x1: D, x1b: Rt3, x2: D, x2b: Rt3): R3 = {
        val r = arg(x1, x1b, x2, x2b)
        f()(r.unboxed, r.boxed)
      }
    }

    val fib: Rt3 = new Rt3 {
      val body: Rt3 =
        if0(x1, num(0.0),
          if1(x1,
            num(1.0),
            plus(
              apDyn(() => body, decrement(x1, 1.0)),
              apDyn(() => body, decrement(x1, 2.0))
            )
          )
        )

      def apply(): R3 = body()
      def apply(x1: D, x1b: Rt3): R3 = body(x1,x1b)
      def apply(x1: D, x1b: Rt3, x2: D, x2b: Rt3): R3 = body(x1,x1b,x2,x2b)
    }

    val fibN: Rt3 = new Rt3 {
      val body = ap(fib, num(N))

      def apply(): R3 = body()
      def apply(x1: D, x1b: Rt3): R3 = body(x1, x1b)
      def apply(x1: D, x1b: Rt3, x2: D, x2b: Rt3): R3 = body(x1, x1b, x2, x2b)
    }

  }

  println(normalize(builtins)(fib))
  println(normalize(builtins)(Compiled(manuallyCompiledFib)(Num(N))))
  println("scala: " + fibScala(N))
  println("Rt2: " + Rt2.fibN(null, R2(null)))
  println("Rt3: " + Rt3.fibN().unboxed)

  val compiledFib = compile(builtins)(fib)

  QuickProfile.suite(
    QuickProfile.timeit("manually-compiled (Rt3)", 0.08) {
      (Rt3.fibN().unboxed + math.random).toLong
    },
    QuickProfile.timeit("manually-compiled (Rt2)", 0.08) {
      (Rt2.fibN(null, R2(null)) + math.random).toLong
    },
    QuickProfile.timeit("unison", 0.08) {
      val r = Result()
      compiledFib(null, r)
      (r.unboxed + math.random).toLong
    },
    QuickProfile.timeit("manually-compiled-unison (2)", 0.08) {
      val r = Result()
      manuallyCompiledFib2(manuallyCompiledFib2, N, null, r)
      (r.unboxed + math.random).toLong
    },
    QuickProfile.timeit("manually-compiled-unison", 0.08) {
      val r = Result()
      manuallyCompiledFib(manuallyCompiledFib, N, null, r)
      (r.unboxed + math.random).toLong
    },
    QuickProfile.timeit("scala", 0.08) {
      (fibScala(N) + math.random).toLong
    }
  )

  // println(normalize(builtins)(fib))
  // println(normalize(builtins)(Num(1.0) + Num(4.0)))
}
