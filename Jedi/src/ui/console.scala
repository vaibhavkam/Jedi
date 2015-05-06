package ui
import values._
import expressions._

/**
 * @author Vaibhav
 *
 */
object console {
   
  val parsers = new Parser
  val globalEnv = new Environment()
  var verbose = true
  val boole = Type.BOOLE
  boole.typ=Type.TYPE
  val number = Type.NUMBER
  number.typ=Type.TYPE
  val integer = Type.INTEGER
  integer.typ=Type.TYPE
  
  def execute(cmmd: String): String = {
    
    val tree = parsers.parseAll(parsers.expression, cmmd)

    tree match {
      case t: parsers.Failure => throw new SyntaxException(t)
      case _ => "" + tree.get.execute(globalEnv)
    }
  }
  
  def repl {
        
    globalEnv.put(new Identifier("Boolean"), boole);
    globalEnv.put(new Identifier("Number"), number);
    
    var cmmd: String = ""
    var more = true
    while (more) {
      try {
        print("-> ")
        val cmmd = readLine()
        if (cmmd == "help") {
          println("help")
        } else if (cmmd == "quit") {
          println("bye")
          more = false
        } else {
          println(execute(cmmd).toString())
        }
      } catch {
        case e: SyntaxException => {
          println(e.msg)
          println(e.result.msg)
          println("line # = " + e.result.next.pos.line)
          println("column # = " + e.result.next.pos.column)
          println("token = " + e.result.next.first)
        }
        case e: TypeException =>
          { println(e.msg); if (verbose) e.printStackTrace() }
        case e: UndefinedException =>
          { println(e.msg); if (verbose) e.printStackTrace() }
        case e: JediException =>
          { println(e.msg); if (verbose) e.printStackTrace() }
        case e: Throwable =>
          { println("error: " + e); if (verbose) e.printStackTrace() }
      } finally {
        Console.flush
      }
    }
  }

  
   def test {
        
    globalEnv.put(new Identifier("Boolean"), boole);
    globalEnv.put(new Identifier("Number"), number); 
  }

  def main(args: Array[String]): Unit = { repl }
}