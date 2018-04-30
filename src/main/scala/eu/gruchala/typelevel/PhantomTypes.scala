package eu.gruchala.typelevel

object PhantomTypes {

}

class Hacker {

  def hackOn: Hacker = {
    println("Hacking, hacking, hacking!")
    new Hacker
  }

  def drinkCoffee: Hacker = {
    println("Slurp ...")
    new Hacker
  }
}
