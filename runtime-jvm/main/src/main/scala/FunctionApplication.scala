package org.unisonweb

import Runtime._
import Term.{Name,Term}
import annotation.switch

object FunctionApplication {

  def staticCall(fn: Rt, args: Array[Rt], decompile: Term, isTail: Boolean): Rt =
    if (isTail) staticTailCall(fn, args, decompile)
    else staticNonTailCall(fn, args, decompile)

  def staticTailCall(fn: Rt, args: Array[Rt], decompile: Term): Rt = ??? /* todo */

  def staticNonTailCall(fn: Rt, args: Array[Rt], decompile: Term): Rt = {
    val arity = args.map(_.arity).max
    val fn2 = fn; val args2 = args
    trait A0 { self: Rt => def bind(env: Map[Name,Rt]) = { fn2.bind(env); args2.foreach(_.bind(env)) }}
    (arity: @switch) match {
      case 0 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity0(decompile) with A0 { def apply(): R = {
            val r = eval(arg0)
            fn(r.unboxed, r.boxed)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity0(decompile) with A0 { def apply(): R = {
            val r0 = eval(arg0); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1); val x2 = r1.unboxed; val x2b = r1.boxed
            fn(x2,x2b,x1,x1b)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity0(decompile) with A0 { def apply(): R = {
            val r0 = eval(arg0); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1); val x2 = r1.unboxed; val x2b = r1.boxed
            val r2 = eval(arg2); val x3 = r2.unboxed; val x3b = r2.boxed
            fn(x3,x3b,x2,x2b,x1,x1b)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity0(decompile) with A0 { def apply(): R = {
            val r0 = eval(arg0); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1); val x2 = r1.unboxed; val x2b = r1.boxed
            val r2 = eval(arg2); val x3 = r2.unboxed; val x3b = r2.boxed
            val r3 = eval(arg3); val x4 = r3.unboxed; val x4b = r3.boxed
            fn(x4,x4b,x3,x3b,x2,x2b,x1,x1b)
          }}
        case n =>
          new Arity0(decompile) with A0 { def apply(): R = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              val r = eval(args(i))
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            fn(slots)
          }}
      }
      case 1 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt): R = {
            val r0 = eval(arg0,a1,a1b); val x1 = r0.unboxed; val x1b = r0.boxed
            fn(x1, x1b)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt): R = {
            val r0 = eval(arg0,a1,a1b); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1,a1,a1b); val x2 = r1.unboxed; val x2b = r1.boxed
            fn(x2,x2b,x1,x1b)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt): R = {
            val r0 = eval(arg0,a1,a1b); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1,a1,a1b); val x2 = r1.unboxed; val x2b = r1.boxed
            val r2 = eval(arg2,a1,a1b); val x3 = r2.unboxed; val x3b = r2.boxed
            fn(x3,x3b,x2,x2b,x1,x1b)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt): R = {
            val r0 = eval(arg0,a1,a1b); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1,a1,a1b); val x2 = r1.unboxed; val x2b = r1.boxed
            val r2 = eval(arg2,a1,a1b); val x3 = r2.unboxed; val x3b = r2.boxed
            val r3 = eval(arg3,a1,a1b); val x4 = r3.unboxed; val x4b = r3.boxed
            fn(x4,x4b,x3,x3b,x2,x2b,x1,x1b)
          }}
        case n =>
          new Arity1(decompile) with A0 { def apply(a1: D, a1b: Rt): R = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              val r = eval(args(i), a1, a1b)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            fn(slots)
          }}
      }
      case 2 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt): R = {
            val r0 = eval(arg0,a1,a1b,a2,a2b); val x1 = r0.unboxed; val x1b = r0.boxed
            fn(x1, x1b)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt): R = {
            val r0 = eval(arg0,a1,a1b,a2,a2b); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1,a1,a1b,a2,a2b); val x2 = r1.unboxed; val x2b = r1.boxed
            fn(x2,x2b,x1,x1b)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt): R = {
            val r0 = eval(arg0,a1,a1b,a2,a2b); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1,a1,a1b,a2,a2b); val x2 = r1.unboxed; val x2b = r1.boxed
            val r2 = eval(arg2,a1,a1b,a2,a2b); val x3 = r2.unboxed; val x3b = r2.boxed
            fn(x3,x3b,x2,x2b,x1,x1b)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt): R = {
            val r0 = eval(arg0,a1,a1b,a2,a2b); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1,a1,a1b,a2,a2b); val x2 = r1.unboxed; val x2b = r1.boxed
            val r2 = eval(arg2,a1,a1b,a2,a2b); val x3 = r2.unboxed; val x3b = r2.boxed
            val r3 = eval(arg3,a1,a1b,a2,a2b); val x4 = r3.unboxed; val x4b = r3.boxed
            fn(x4,x4b,x3,x3b,x2,x2b,x1,x1b)
          }}
        case n =>
          new Arity2(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt): R = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              val r = eval(args(i), a1, a1b, a2, a2b)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            fn(slots)
          }}
      }
      case 3 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt): R = {
            val r = eval(arg0,a1,a1b,a2,a2b,a3,a3b)
            fn(r.unboxed, r.boxed)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt): R = {
            val r0 = eval(arg0,a1,a1b,a2,a2b,a3,a3b); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1,a1,a1b,a2,a2b,a3,a3b); val x2 = r1.unboxed; val x2b = r1.boxed
            fn(x2,x2b,x1,x1b)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt): R = {
            val r0 = eval(arg0,a1,a1b,a2,a2b,a3,a3b); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1,a1,a1b,a2,a2b,a3,a3b); val x2 = r1.unboxed; val x2b = r1.boxed
            val r2 = eval(arg2,a1,a1b,a2,a2b,a3,a3b); val x3 = r2.unboxed; val x3b = r2.boxed
            fn(x3,x3b,x2,x2b,x1,x1b)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt): R = {
            val r0 = eval(arg0,a1,a1b,a2,a2b,a3,a3b); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1,a1,a1b,a2,a2b,a3,a3b); val x2 = r1.unboxed; val x2b = r1.boxed
            val r2 = eval(arg2,a1,a1b,a2,a2b,a3,a3b); val x3 = r2.unboxed; val x3b = r2.boxed
            val r3 = eval(arg3,a1,a1b,a2,a2b,a3,a3b); val x4 = r3.unboxed; val x4b = r3.boxed
            fn(x4,x4b,x3,x3b,x2,x2b,x1,x1b)
          }}
        case n =>
          new Arity3(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt): R = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              val r = eval(args(i), a1, a1b, a2, a2b, a3, a3b)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            fn(slots)
          }}
      }
      case 4 => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt): R = {
            val r = eval(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b)
            fn(r.unboxed, r.boxed)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt): R = {
            val r0 = eval(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b); val x2 = r1.unboxed; val x2b = r1.boxed
            fn(x2,x2b,x1,x1b)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt): R = {
            val r0 = eval(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b); val x2 = r1.unboxed; val x2b = r1.boxed
            val r2 = eval(arg2,a1,a1b,a2,a2b,a3,a3b,a4,a4b); val x3 = r2.unboxed; val x3b = r2.boxed
            fn(x3,x3b,x2,x2b,x1,x1b)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt): R = {
            val r0 = eval(arg0,a1,a1b,a2,a2b,a3,a3b,a4,a4b); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = eval(arg1,a1,a1b,a2,a2b,a3,a3b,a4,a4b); val x2 = r1.unboxed; val x2b = r1.boxed
            val r2 = eval(arg2,a1,a1b,a2,a2b,a3,a3b,a4,a4b); val x3 = r2.unboxed; val x3b = r2.boxed
            val r3 = eval(arg3,a1,a1b,a2,a2b,a3,a3b,a4,a4b); val x4 = r3.unboxed; val x4b = r3.boxed
            fn(x4,x4b,x3,x3b,x2,x2b,x1,x1b)
          }}
        case n =>
          new Arity4(decompile) with A0 { def apply(a1: D, a1b: Rt, a2: D, a2b: Rt, a3: D, a3b: Rt, a4: D, a4b: Rt): R = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              val r = eval(args(i), a1, a1b, a2, a2b, a3, a3b, a4, a4b)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            fn(slots)
          }}
      }
      case n => (args.length: @switch) match {
        case 1 =>
          val arg0 = args(0)
          new ArityN(n, decompile) with A0 { def apply(args: Array[Slot]): R = {
            val r = evalN(arg0, args)
            fn(r.unboxed, r.boxed)
          }}
        case 2 =>
          val arg0 = args(0)
          val arg1 = args(1)
          new ArityN(n, decompile) with A0 { def apply(args: Array[Slot]): R = {
            val r0 = evalN(arg0, args); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = evalN(arg1, args); val x2 = r1.unboxed; val x2b = r1.boxed
            fn(x2,x2b,x1,x1b)
          }}
        case 3 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          new ArityN(n, decompile) with A0 { def apply(args: Array[Slot]): R = {
            val r0 = evalN(arg0, args); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = evalN(arg1, args); val x2 = r1.unboxed; val x2b = r1.boxed
            val r2 = evalN(arg2, args); val x3 = r2.unboxed; val x3b = r2.boxed
            fn(x3,x3b,x2,x2b,x1,x1b)
          }}
        case 4 =>
          val arg0 = args(0)
          val arg1 = args(1)
          val arg2 = args(2)
          val arg3 = args(3)
          new ArityN(n, decompile) with A0 { def apply(args: Array[Slot]): R = {
            val r0 = evalN(arg0, args); val x1 = r0.unboxed; val x1b = r0.boxed
            val r1 = evalN(arg1, args); val x2 = r1.unboxed; val x2b = r1.boxed
            val r2 = evalN(arg2, args); val x3 = r2.unboxed; val x3b = r2.boxed
            val r3 = evalN(arg3, args); val x4 = r3.unboxed; val x4b = r3.boxed
            fn(x4,x4b,x3,x3b,x2,x2b,x1,x1b)
          }}
        case m =>
          new ArityN(n, decompile) with A0 { def apply(args0: Array[Slot]): R = {
            val slots = new Array[Slot](args.length)
            var i = 0
            while (i < slots.length) {
              val slot = slots(slots.length - 1 - i)
              val r = evalN(args(i), args0)
              slot.unboxed = r.unboxed
              slot.boxed = r.boxed
              i += 1
            }
            fn(slots)
          }}
      }
    }
  }
}

