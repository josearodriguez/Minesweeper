package es.buscaminas

import scala.io.StdIn

class Juego(val filas: Int, val columnas: Int, val nMinas: Int) {
  private val tablero: Tablero = new Tablero(filas, columnas, nMinas)
  Utilidades.pintarMatrizValores(tablero.casillas)
  def jugar(): Unit = {
    Utilidades.pintarMatrizEstados(tablero.casillas)

    val accion = Utilidades.leer()
    val casilla = tablero.casillas(accion._2)(accion._3)
    accion match {
      case ("r", _, _) => tablero.descubrir(List(casilla))
      case ("f", _, _) => tablero.marcar(casilla)
      case _           => println(s"El comando no es correcto, usa: r - desvelar, f - marcar")
    }

    
    if (victoria())
      println("Enhorabuena!!, Has ganado!")
    else if (derrota()) {
      Utilidades.pintarMatrizValores(tablero.casillas)
      println("Fin de la partida, ha explotado una bomba!")
    } else
      jugar()
  }

  def victoria(): Boolean = tablero.casillas.flatten.filter(p=>p.valor==9 && p.estado == Visible).size == tablero.minas.size
  
  def derrota(): Boolean = !tablero.casillas.flatten.filter(p=>p.valor==9 && p.estado == Visible).isEmpty

}