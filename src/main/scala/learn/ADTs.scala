package learn

object ADTs {


  // ways of structuring your data

  sealed trait Weather // "Sum type"
  case object Sunny extends Weather
  case object Windy extends Weather
  case object Rainy extends Weather

  def feeling( weather: Weather):String = weather match {
    case Sunny => ":)"
    case Windy => ":|"
  }

  case class WeatherForecastRequest(latitude: Double, longitude:Double)
  // (Double, Double) => WFR
  // type WFR = Double x Double (cartesian product between Double and Double) = Product type = implemented in Scala as case classes

  // hybrid types
    sealed trait WeatherForecastResponse // SUM type
  case class Valid(weather: Weather) extends WeatherForecastResponse
  case class Invalid(error: String, description: String) extends WeatherForecastResponse
}
