package es.buscaminas

import scala.io.StdIn

class Juego(val filas: Int, val columnas: Int, val nMinas: Int) {
  private val tablero: Tablero = new Tablero(filas, columnas, nMinas)
  Utilidades.pintarMatrizValores(tablero.casillas)
  def jugar(): Unit = {
    Utilidades.pintarMatrizEstados(tablero.casillas)

    val accion = Utilidades.leer()

    accion match {
      case ("r", _, _) => tablero.desvelar(List((accion._2, accion._3)))
      case ("f", _, _) => tablero.marcar(accion._2, accion._3)
      case ("q", _, _) => tablero.cuestionar(accion._2, accion._3)
      case _           => println(s"El comando no es correcto, usa: r - desvelar, f - marcar, q- cuestionar")
    }

    if (tablero.victoria())
      println("Enhorabuena!!, Has ganado!")
    else if (tablero.derrota()) {
      Utilidades.pintarMatrizValores(tablero.casillas)
      println("Fin de la partida, ha explotado una bomba!")
    } else
      jugar()
  }
}