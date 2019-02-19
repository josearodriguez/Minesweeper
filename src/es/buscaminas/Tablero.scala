package es.buscaminas
import scala.annotation.tailrec
import scala.util.Random
import scala.math.Ordering.Implicits._
import scala.math._

class Tablero(val cFilas: Int, val cColumnas: Int, val cMinas: Int) {
  var campo = new Handler()
  var minas = generarMinas()
  var casillas: List[List[Casilla]] = crearCasillas();

  def filas = cFilas-1
  def columnas = cColumnas -1

  /**
   * TABLERO
   * [ [FILA 0], [FILA 1], [FILA 2]]
   * */
  
  def generarMinas(): Set[Tuple2[Int, Int]] = (0 to cMinas).map(i => campo.getUniqueTuple()).toSet
  
  def crearCasillas(): List[List[Casilla]] = (0 to filas).map(x => (0 to columnas).map(y => new Casilla(x,y,Oculta,getValor(x, y))).toList).toList

  def distanciaDosPuntos(a: Tuple2[Int, Int], b: Tuple2[Int, Int]): Double = sqrt(pow(a._1 - b._1, 2) + pow(a._2 - b._2, 2))

  def esVecino(a: Tuple2[Int, Int], b: Tuple2[Int, Int]): Boolean = distanciaDosPuntos(a, b) == sqrt(1) || distanciaDosPuntos(a, b) == sqrt(2)

  def posicionesTablero: List[(Int, Int)] = (0 to filas).map(x => (0 to columnas).map(y => Tuple2(x, y))).toList.flatten

  def obtenerVecinos(c:Casilla): List[(Int, Int)] = posicionesTablero.filter(p => esVecino((c.posX, c.posY), (p._1, p._2)))

  def obtenerVecinosNoVisibles(c:Casilla): List[(Int, Int)] = obtenerVecinos(c).filter(p=> casillas(p._1)(p._2).estado!=Visible)
  
  def getValor(x: Int, y: Int): Int = {
    if (minas.contains(x, y)) return 9 //Bomb
    else {
      val minasCerca = minas.filter(i => esVecino((x, y), i))
      return minasCerca.size
    }
  }
  
  def cambiaVisibilidadCasilla(visibilidad:EstadoCasilla, c:Casilla):Unit = {
    c.estado = visibilidad  
    casillas.updated(c.posX, casillas(c.posX).updated(c.posY, c))
    
  }
  
  final def desvelar(puntos : List[(Int,Int)] ): Unit = {
    if (puntos.nonEmpty){
      val casillaActual = casillas(puntos.head._1)(puntos.head._2)
      cambiaVisibilidadCasilla(Visible,casillaActual)
      val estadoCasilla = casillaActual.estado
      casillaActual.valor match {
        case 0 => desvelar((puntos.tail:::obtenerVecinosNoVisibles(casillaActual)).toSet.toList)
        case _  =>desvelar(puntos.tail)
      }
    }
  }
  
  def marcar(x: Int, y: Int): Unit = {
    if (casillas(x)(y).estado != Visible)
      cambiaVisibilidadCasilla(Marcada,casillas(x)(y))
  }

  def cuestionar(x: Int, y: Int): Unit = {
    if (casillas(x)(y).estado != Questionada)
      cambiaVisibilidadCasilla(Marcada,casillas(x)(y))
  }

  def victoria(): Boolean = {
    casillas.map(i => i.filter(j => j.estado == Oculta).size).sum == minas.size
  }

  def derrota(): Boolean = !minas.filter(i => casillas(i._1)(i._2).estado == Visible).isEmpty
    
  class Handler {
    var saved: List[(Int, Int)] = List()

    @tailrec final def getUniqueTuple(): (Int, Int) = {
      val t = (Random.nextInt(filas), Random.nextInt(columnas))
      if (saved.contains(t)) getUniqueTuple()
      else {
        saved = t :: saved
        t
      }
    }
  }
}