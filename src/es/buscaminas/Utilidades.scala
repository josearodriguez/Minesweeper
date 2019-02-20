package es.buscaminas

import scala.io.StdIn
import scala.util.Try

object Utilidades {
  def pintarMatrizValores(casillas: List[List[Casilla]]): Unit = {
    casillas.foreach(f => {
      f.foreach(casilla => print(casilla.valor + "|"))
      println()
    })
    println()
  }

  def pintarMatrizEstados(casillas: List[List[Casilla]]): Unit = {
    casillas.foreach(f => {
      f.foreach(c => {
        c.estado match {
          case Oculta      => print("X|")
          case Marcada     => print("?|")
          case Visible     => print(c.valor + "|")
          case _           => print(c.valor + "|")
        }

      })
      println();
    })
  }
  def leer(): (String, Int, Int) = {
    println("[comando] [fila] [columna]")
    val linea = StdIn.readLine()

    val accion = linea.split(" ").map(i => i)
    if (accion.size == 3 && !tryToInt(accion(1)).isEmpty && !tryToInt(accion.last).isEmpty 
        && accion(1).toInt > 0 && accion(2).toInt > 0)
      return (accion.head, accion(1).toInt - 1, accion.last.toInt - 1)
    leer()
  }
  
  private def tryToInt(s: String) = Try(s.toInt).toOption

}