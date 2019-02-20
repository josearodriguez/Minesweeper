package es.buscaminas
import scala.annotation.tailrec
import scala.util.Random
import scala.math.Ordering.Implicits._
import scala.math._

class Tablero(val cFilas: Int, val cColumnas: Int, val cMinas: Int) {
  var generadorMinas = new GeneradorMinas(cFilas, cColumnas)
  var minas = generarMinas()
  var casillas: List[List[Casilla]] = crearCasillas();

  /**
   * TODO Crear una Lista de Lista con objetos Casillas
   */
  def crearCasillas(): List[List[Casilla]] = List.tabulate(cFilas, cColumnas)(nuevaCasillaOculta)
  def nuevaCasillaOculta(f: Int, c: Int): Casilla = new Casilla(f, c, Oculta, getValorCasilla(f, c))

  /**
   * TODO Funcion que descubre una casilla y todas las colindantes que tengan valor 0, recursivamente
   *
   */
  final def descubrir(casillas: List[Casilla]): Unit = {
    if (casillas.nonEmpty) {
      val casillaActual = casillas.head
      cambiaVisibilidadCasilla(Visible, casillaActual)
      val estadoCasilla = casillaActual.estado
      casillaActual.valor match {
        case 0 => descubrir((casillas.tail ::: obtenerVecinosNoVisibles(casillaActual)).toSet.toList)
        case _ => descubrir(casillas.tail)
      }
    }
  }

  /**
   * TODO Crear una funcion que devuelva los vecinos de una casilla
   */
  def obtenerVecinos(c: Casilla): List[Casilla] = casillas.flatten.filter(p => esVecino((c.posX, c.posY), (p.posX, p.posY)))

  /**
   * TODO Crea una funcion que devuelva todos los vecinos visibles de una casilla
   */
  def obtenerVecinosNoVisibles(c: Casilla): List[Casilla] = obtenerVecinos(c).filter(p => p.estado != Visible)

  /**
   * Cambia la visibilidad de una casilla
   */
  def cambiaVisibilidadCasilla(visibilidad: EstadoCasilla, c: Casilla): Unit = {
    c.estado = visibilidad
    casillas.updated(c.posX, casillas(c.posX).updated(c.posY, c))
  casillas(c.posX)(c.posY)
  }
  /**
   * Cambia el valor de una casilla
   */
  def marcar(casilla: Casilla): Unit = if (casilla.estado != Visible) cambiaVisibilidadCasilla(Marcada, casilla)

  /**
   * TABLERO
   * [ [FILA 0], [FILA 1], [FILA 2]]
   */

  def generarMinas(): Set[(Int, Int)] = (0 to cMinas - 1).map(i => generadorMinas.obtenerPosicionMina()).toSet

  def distanciaDosPuntos(a: (Int, Int), b: (Int, Int)): Double = sqrt(pow(a._1 - b._1, 2) + pow(a._2 - b._2, 2))

  def esVecino(a: (Int, Int), b: (Int, Int)): Boolean = distanciaDosPuntos(a, b) == sqrt(1) || distanciaDosPuntos(a, b) == sqrt(2)

  /**
   * Calcula el valor de una casilla segun las bombas que tenga a su alrededor
   */
  def getValorCasilla(x: Int, y: Int): Int = {
    if (minas.contains(x, y)) return 9 //Bomb
    else {
      val minasCerca = minas.filter(i => esVecino((x, y), i))
      return minasCerca.size
    }
  }

}