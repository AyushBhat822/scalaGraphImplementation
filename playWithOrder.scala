package learningGraph

object playWithOrder extends App{
  //Example 1

  // Define a list of tuples
  val lst = List((3, "c"), (1, "a"), (4, "d"), (2, "b"))

  // Sort the list by the first element of each tuple in ascending order
  // projection function _._1 to extract the first element of each pair 
  // and compares them using the default ordering for integers
  val sortedLst = lst.sorted(Ordering.by(_._1))

  // Print the sorted list
  println(sortedLst)

  //Example2
  // Define the list of tuples
  val people = List(("Alice", 25), ("Bob", 20), ("Charlie", 30))

  // Define the custom ordering based on the second element of the tuples (age)
  implicit val ordering: Ordering[(String, Int)] = Ordering.by(_._2)

  // Sort the list using the custom ordering
  val sortedPeople = people.sorted

  // Print the sorted list
  println(sortedPeople)

}
