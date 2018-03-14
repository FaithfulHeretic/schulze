package org.liberalist.schulze.prototype

object JustifyUtil {
  def rightJustify(s: String, width: Int, padding: String = " "): String = {
    paddingFor(s, width, padding) + s
  }

  def leftJustify(s: String, width: Int, padding: String = " "): String = {
    s + paddingFor(s, width, padding)
  }

  private def paddingFor(s: String, width: Int, padding: String): String = {
    val quantity = width - s.length
    padding * quantity
  }
}