package es.buscaminas

class Casilla(cx:Int,cy:Int, cestado: EstadoCasilla, cvalor: Int) {
  var estado: EstadoCasilla = cestado
  var valor: Int = cvalor
  var posX: Int = cx
  var posY: Int = cy

  override def toString(): String = s"($posX , $posY) $estado : $valor"
}