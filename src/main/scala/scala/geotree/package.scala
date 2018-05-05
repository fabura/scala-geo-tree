package scala

import scala.concurrent.Future
import scala.geotree.DistUnits.Measurement
import scala.util.{Failure, Success, Try}

package object geotree {

  object DistUnits{
    sealed trait Measurement

    case object Meter extends Measurement
    case object Kilometer extends Measurement
    case object Mile extends Measurement
    case object Feet extends Measurement
  }


  case class Distance(value: Double, measurement: Measurement){
    def toMeters: Double = measurement match {
      case DistUnits.Meter => value
      case DistUnits.Kilometer => value * 1000
      case DistUnits.Mile => value * 1602
      case DistUnits.Feet => value * 0.32
    }
  }

  object Distance {

    private val regex = "(^\\d+(?:\\.\\d+)?)(m|km|mi|ft|)$".r
    private val failure: Try[Nothing] = Failure(new RuntimeException("Wrong distance format. Try <number><[m]|km|mi|ft>"))

    def fromString(str: String): Try[Distance] = str match {
      case regex(r: String, measurement: String) =>
        val m = measurement match {
          case "m" => Success(DistUnits.Meter)
          case "km" => Success(DistUnits.Kilometer)
          case "ft" => Success(DistUnits.Feet)
          case "mi" => Success(DistUnits.Mile)
          case "" => Success(DistUnits.Meter)
          case _ => failure
        }
        m.map(m => Distance(r.toDouble, m))
      case _ => failure
    }
  }

  implicit val distanceOrdering: Ordering[Distance] = Ordering.by(d => d.toMeters)

  type Degree = Double
  type Longitude = Degree
  type Latitude = Degree

  case class Place(name: String, distance: Distance, longitude: Degree, latitude: Degree)

  object Place {
    def parseFromStrings(strings: Seq[String], measurement: Measurement): Iterator[Place] = {
      strings.sliding(4, 4).map {
        s =>
          Place(s.head, Distance(s(1).toDouble, measurement), s(2).toDouble, s(3).toDouble)
      }
    }
  }

}
