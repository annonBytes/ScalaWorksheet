
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Random, Success}

object FutureBaristaLecture extends App {

  case class Water(temperature: Int)
  case class CoffeeBeans(name: String, grounded: Boolean = false)
  case class Espresso(beans: CoffeeBeans, water: Water)
  case class Milk(frothed: Boolean = false)
  case class Cappuccino(espresso: Espresso, milk: Milk)

  case class HeatingException(msg: String) extends Exception(msg)
  case class GrindingException(msg: String) extends Exception(msg)
  case class BrewingException(msg: String) extends Exception(msg)
  case class CappuccinoException(msg: String) extends Exception(msg)

  def heat(water: Water) = Future {
    println("start heating")
    if (water.temperature > 100) throw HeatingException("thermostat broken")

    Thread.sleep(1000 + Random.nextInt(1000))
    println("done heating")
    water.copy(temperature = 95)
  }

  def grind(beans: CoffeeBeans) = Future {
    println("start grinding")
    if (beans.name == "Baked Beans") throw GrindingException("wrong kind of beans")

    Thread.sleep(500 + Random.nextInt(500))
    println("done grinding")
    beans.copy(grounded = true)
  }

  def brew(beans: CoffeeBeans, water: Water) = Future {
    println("start brewing")
    if (!beans.grounded) throw BrewingException ("beans are not grounded")
    if (water.temperature < 80) throw BrewingException ("water too cold")
    Thread.sleep(750 + Random.nextInt(750))
    println("done brewing")
    Espresso(beans, water)
  }

  def froth(milk: Milk) = Future {
    println("milk frothing system engaged!")
    Thread.sleep(800 + Random.nextInt(800))
    println("shutting down milk frothing system")
    Milk(frothed = true)
  }

  def combine(espresso: Espresso, milk: Milk):Future[Cappuccino] = Future {
    println("combining the cappuccino with frothed milk")
    Thread.sleep(850 + Random.nextInt(850))
    println("stopping combination")
    Cappuccino(espresso, milk)
  }

  var beans1 = CoffeeBeans("Arabica")
  var beans2 = CoffeeBeans("Baked Beans")

  var water1 = Water(20)
  var water2 = Water(150)

  var milk2 = Milk(false)
  var milk1 = Milk(true)


  def coffeeCallback(beans: CoffeeBeans, water: Water) = {
    heat(water).onComplete{
      case Success(heatedWater) =>
        println("Water Temperature: " + heatedWater.temperature)
        grind(beans).onComplete {
          case Success(groundedBeans) =>
            println("Beans grounded: " + groundedBeans.grounded)
            brew(groundedBeans, heatedWater).onComplete {
              case Success(myEspresso) =>
                println(myEspresso)
              case Failure(exception: Exception) => println(exception.getMessage)
            }
          case Failure(exception: Exception) => println(exception.getMessage)
        }
      case Failure(exception: Exception) => println(exception.getMessage)
    }
  }

  def prepareEspresso(beans: CoffeeBeans, water: Water) = {
    for {
      heatedWater <- heat(water)
      groundedBeans <- grind(beans)
      espresso <- brew(groundedBeans, heatedWater)
      foam <- froth(milk1)
    } yield combine(espresso, foam)
  }

  def prepareEspressoParallel(beans: CoffeeBeans, water: Water) = {
    val waterHeating = heat(water)
    val beanGrinding = grind(beans)
    for {
      heatedWater <- waterHeating
      groundedBeans <- beanGrinding
      espresso <- brew(groundedBeans, heatedWater)
    } yield espresso
  }

  def prepareEspressoMap(beans: CoffeeBeans, water: Water): Unit = {
    grind(beans).flatMap(grounded =>
       heat(water).flatMap(heated =>
         brew(grounded, heated)
       ))
  }

  def prepareEspressoParallelMap(beans: CoffeeBeans, water: Water): Unit = {
    val waterHeating = heat(water)
    val beanGrinding = grind(beans)

    beanGrinding.flatMap(grounded =>
    waterHeating.flatMap(heated =>
      brew(grounded,heated)
    ))
  }

  def prepareCappuccinoMap(espresso: Espresso, milk: Milk): Unit = {
    froth(milk).flatMap(frothedMilk =>
    combine(espresso,frothedMilk))
  }

  def prepareCappuccinoMapParallel(espresso: Espresso, milk: Milk): Unit = {
     val froths = froth(milk)

    froths.flatMap(frothedMilk =>
    combine(espresso,frothedMilk))
  }


  // for printing exceptions
  val cappuccino = prepareEspresso(beans1, water1) recover {
    case e => println(e.getMessage)
  }

  //  val cappuccino = prepareEspresso(beans1, water1) recover {
  //    case e => println(e.getMessage)
  //  }


//    val my_espresso = prepareEspressoParallelMap(beans1, water1) recover {
//      case e => println(e.getMessage)
//    }



  Thread.sleep(5000)
}


