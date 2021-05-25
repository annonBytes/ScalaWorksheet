import scala.util.{Failure, Random, Success}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global





object FutureBarista extends  App {
  case class Water (temperature: Int)
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
    if(water.temperature > 100) throw HeatingException("thermostat broken")
    Thread.sleep(1000 + Random.nextInt(1000))
    println("done heating")
    water.copy(temperature = 95)
  }


  def grind(beans: CoffeeBeans) = Future {
    println("start grinding")
    if(beans.name == "Baked Beans") throw GrindingException("wrong kind of beans")
    Thread.sleep(500 + Random.nextInt(500))
    println("done grinding")
    beans.copy(grounded = true)
  }

  def brew(beans: CoffeeBeans, water: Water) = Future {
    println("start brewing")
    if(water.temperature < 80) throw BrewingException("the water is too cold")
    if(!beans.grounded) throw BrewingException("beans are not grounded")
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

  val beans1 = CoffeeBeans("Arabica")
  val beans2 = CoffeeBeans("Bakeed Beans")

  val water1 = Water(20)
  val water2 = Water(120)

  def coffeeCallback(beans: CoffeeBeans, water: Water) = {
     heat(water).onComplete {
       case Success(hotWater) =>
         println(hotWater.temperature)
         grind(beans).onComplete {
           case Success(groundedBeans) =>
             brew(groundedBeans, hotWater).onComplete {
               case Success(myEspresso) => println(myEspresso)
               case Failure(exception) => println(exception.getMessage)
             }
           case Failure(exception) => println(exception.getMessage)
         }
       case Failure(exception) => println(exception.getMessage)
     }
  }

  def prepareEspresso(beans: CoffeeBeans, water: Water): Unit = {
    for {
      hotWater <- heat(water)
      groundedBeans <- grind(beans)
      espresso <- brew(groundedBeans, hotWater)
    } yield espresso
  }

  def prepareEspressoParallel(beans: CoffeeBeans, water: Water): Unit = {
    val waterHeating = heat(water)
    val beanGrinding = grind(beans)
    for {
      hotWater <- waterHeating
      groundedBeans <- beanGrinding
      espresso <- brew(groundedBeans, hotWater)
    } yield espresso
  }

  def preparingEspresso(beans: CoffeeBeans, water: Water): Unit = {
    for {
      hotWater <- heat(water)
      groundedBeans <- grind(beans)
      espresso <- brew(groundedBeans, hotWater)
    } yield espresso
  }

//  val myEspresso = prepareEspresso(beans1, water2) recover  {
//    case e => println(e.getMessage)
//  }

  // coffeeCallback(beans1, water1)

  Thread.sleep(5000)
}
