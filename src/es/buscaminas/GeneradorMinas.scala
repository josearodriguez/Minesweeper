package es.buscaminas
import scala.annotation.tailrec
import scala.util.Random
import scala.math.Ordering.Implicits._
import scala.math._

class GeneradorMinas(f:Int,c:Int) {
   var saved: List[(Int, Int)] = List()

    @tailrec final def obtenerPosicionMina(): (Int, Int) = {
      val t = (Random.nextInt(f), Random.nextInt(c))
      if (saved.contains(t)) obtenerPosicionMina()
      else {
        saved = t :: saved
        t
      }
    }
}