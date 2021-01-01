package org.clulab.processors

import edu.cmu.dynet.Dim
import edu.cmu.dynet.Expression
import edu.cmu.dynet.ExpressionVector
import org.clulab.fatdynet.utils.Initializer

/*
// In C++
Expression is just a
  ComputationGraph *pg
  VariableIndex i

ExpressionVector is std::vector<ComputationGraph>

// In Java
Expression is
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;
so that swigCPtr points straight at the C++ memory of the object.

If it is deleted in C++, that swigCPtr is invalid.
If it is deleted in Java, the memory is freed and any C++ pointers are invalid.

ExpressionVector is
  public transient long swigCPtr;
  protected transient boolean swigCMemOwn;
Again, pointers to the real C++ pointer.  Upon finalization, the C++ object will be deleted.


// In Scala
Expression in
  val expr: internal.Expression,
  val references: Seq[AnyRef] = Seq.empty
references are things that the expression should keep alive so that it can work.

ExpressonVector
  val version: Long,
  val vector: internal.ExpressionVector
Note that this is again a "have a" relationship with the Java version.

*/

object TestExpressions extends App {
  Initializer.initialize()

  val dim = Dim(1)
  val value = 3.14f
  val ex = Expression.constant(dim, value)
//  val cg = ex.expr.getPg

/*
  val (ex1, ev1) = {
    val ex = Expression.constant(dim, value)
    val cg1 = ex.expr.getPg

    // The underlying C++ will get a copy of ex.
    val ev = new ExpressionVector(Seq(ex))
    val cg2 = ev(0).expr.getPg

    ex.expr.delete()
    // val cg3 = ex.expr.getPg // This crashes.
    val cg4 = ev(0).expr.getPg // This works on the copy.

    ev(0).expr.delete() // This doesn't seem to do anything because ev(0) doesn't own the memory.
    val cg5 = ev(0).expr.getPg // This continues to work.

//    ev.vector.delete() // This will delete ev(0) from above so that there is no memory leak.
    // There will be a crash in the C++ code shortly after this double delete.

    //val cg6 = ev(0).expr.getPg // This crashes because the vector isn't even there.

    println("The test is finished.")
    (ex, ev)
  }
*/

//  val (ex2, ev2) =
  {
    val ex = Expression.constant(dim, value)
//    val cg1 = ex.expr.getPg

    val ev = new ExpressionVector(Seq(ex))
//    val cg2 = ev(0).expr.getPg

    // This does delete the one expression in the vector, but it is a copy.
//    ev.vector.delete() // Something goes wrong.  GC?
    // There will be a crash in the C++ code shortly after this double delete.
    // C++ tries to take size of vector which results in access violation.

//    val cg3 = ex.expr.getPg // This works on the original copy.
//    val cg4 = ev(0).expr.getPg // This crashes because the vector isn't even there.

//    println("The test is finished.")
//    (ex, ev)
  }

  // Additional garbage collection could take place during/after these.
  // These will hit at finalize rather than at delete.
//  println(ex1)
//  println(ev1)
//  println(ex2)
//  println(ev2)
}
