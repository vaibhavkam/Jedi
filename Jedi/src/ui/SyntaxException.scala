/**
 *
 */
package ui

import scala.util.parsing.combinator._

/**
 * @author Vaibhav
 *
 */
class SyntaxException(val result: Parsers#Failure = null) extends JediException("Syntax error") {

}