package es.buscaminas


trait EstadoCasilla 

case object Visible extends EstadoCasilla
case object Oculta extends EstadoCasilla
case object Marcada extends EstadoCasilla
case object Cuestionada extends EstadoCasilla
