/**
 *
 */
package ui

/**
 * @author Vaibhav
 *
 */
class UndefinedException(val symbol: String) extends JediException("Undefined identifier: " + symbol) {

}