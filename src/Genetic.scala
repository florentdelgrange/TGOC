import scala.util.Random
import scala.io.Source

class Genetic{
  var parentList  = List(0)
  var popList  = List(0)
  var childList = List(0)
//  var graph : Graphe
  def initGraphe(file : String){
    val lines = Source.fromFile(file).getLines().toList

    val info = lines(0).split(" ")
    val head = lines(1).split(" ")
    val succ = lines(2).split(" ")
    val coordx = lines(3).split(" ")
    val coordy = lines(4).split(" ")
    val distance = lines(5).split(" ")
    val prospectus = lines(6).split(" ")

    println("Arc " + info(0) + " Sommets "+info(1)+" Equipes  "+info(2))
    println("HEAD : "+head.foreach(println))
    println("SUCC : "+succ)
    println("COORD X : "+coordx)
    println("Distance : "+distance)
    println("Prospectus : "+prospectus)
  }
  def fitness(sub : Int,list : List[Int]) = sub.toFloat / list.view.map(_.toLong).sum
  def initPopulation(popSize : Int){ popList = List(4,3,2,1,100) }

//  def poplationEval()
  def selectParent(nbrCross : Int){
    // Avant de proceder a la selection on reinitialise les parents selectionnés
    parentList = List()
    //On utilise une copie de la liste d'invidu pour plus tard ne pas reprendre
    //deux fois le meme individu
    var popListSort = popList
    //On va selection N individu a croiser N etant le parametre
    for ( i <- 0.to(nbrCross-1) ){
      println("PopList : "+popListSort)
      // On génére un nombre alléatoire ( on lance la roulette qui est divisée
      // selon la fonction fitness de chaque element
      val rand = new Random().nextFloat()
      println(rand)
      //Si le on a prob < rand < prob+fitness(subject) c'est que l'on se trouve
      //sur le quartier de la roulette correspondant a l'individu subject
      //sinon on ajoute fitness(sub) a prob pour passer au second quartier ainsi
      //de suiste jusqu'a ce qu'on trouve le bon element a ajouter au parent
      var prob = 0.
      popListSort.foreach(sub =>
          if(rand < prob + fitness(sub,popListSort) && rand > prob ){
            parentList = sub :: parentList
            popListSort = popListSort.filter(s => s!=sub)
            println("Elem Added "+sub)
            prob+= fitness(sub,popListSort)
          }
          else{
            println("Elem " + sub +" Prob : "+prob+" Fit: "+fitness(sub,popListSort))
            prob+= fitness(sub,popListSort)
          })
          //On a donc mtn une liste de N individu pret a etre croiser
    }

  }
//  def croissing()
//  def addNewPopulation()

}
object Genetic{
  def main(args:Array[String]){
    val gen = new Genetic()
    gen.initGraphe("input.txt")
    gen.initPopulation(6)
    gen.selectParent(2)
    println(gen.parentList)
  }
}
