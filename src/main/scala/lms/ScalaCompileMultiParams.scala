package lms

import scala.lms.internal._
import java.io._
import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import scala.tools.nsc.util.AbstractFileClassLoader

trait ScalaCompileMultiParams extends ScalaCompile {
  override val codegen: ScalaCodegen { val IR: ScalaCompileMultiParams.this.type }

  def freshClass = {
     val className = "staged$" + compileCount;
     compileCount += 1;
     className
  }

  def compileAny(className: String, staticData: List[(Sym[Any], Any)], source: String): Any = {
    if (this.compiler eq null)
      setupCompiler()

     println(source)

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)

    run.compileSources(List(new util.BatchSourceFile("<stdin>", source)))
    reporter.printSummary()

    if (reporter.hasErrors)
      println(s"compilation $className: completed with errors")

    reporter.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure): _*)

    cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]): _*)
  }

  override def compile[A, R](f: Exp[A] => Exp[R]) (implicit mA: Manifest[A], mR: Manifest[R]): A => R = {
    val className = freshClass
    val source = new StringWriter()
    val staticData = codegen.emitSource(f, className, new PrintWriter(source))

    compileAny(className, staticData, source.toString).asInstanceOf[A => R]
  }

  def compile2[A, B, R](f: (Exp[A], Exp[B]) => Exp[R]) (implicit mA: Manifest[A], mB: Manifest[B], mR: Manifest[R]): (A, B) => R = {
    val className = freshClass
    val source = new StringWriter()
    val staticData = codegen.emitSource2(f, className, new PrintWriter(source))

    compileAny(className, staticData, source.toString).asInstanceOf[(A, B) => R]
  }

  def compile3[A, B, C, R](f: (Exp[A], Exp[B], Exp[C]) => Exp[R]) (implicit mA: Manifest[A], mB: Manifest[B], mC: Manifest[C], mR: Manifest[R]): (A, B, C) => R = {
    val className = freshClass
    val source = new StringWriter()
    val staticData = codegen.emitSource3(f, className, new PrintWriter(source))

    compileAny(className, staticData, source.toString).asInstanceOf[(A, B, C) => R]
  }
}
