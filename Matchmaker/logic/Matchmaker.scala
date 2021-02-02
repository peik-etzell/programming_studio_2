package logic

import scala.collection.mutable.Buffer

class Matchmaker(personData: Array[Person]) {
  
  /**
   * Uses the people recorded in the matchmaker (personData) to create a map from each possible pairing
   * to the mutual matchmaking score of that pair.
   * 
   * When creating the map remember to take each pair of people *only once*, 
   * e.g. take only (Matt, Laura) or (Laura, Matt). If the person appears earlier in the list
   * you should put them as the first half of the pair.
   */
  def matchMap: Map[(Person, Person), Int] = {
    for {
      person1 <- personData
      person2 <- personData.drop(personData.indexOf(person1) + 1)
    } yield (person1, person2) -> person1.bothMatch(person2)
  }.toMap


  def matchMapCalle: Map[(Person, Person), Int] = {
    var map = Map[(Person, Person), Int]()
    for (person <- personData){
      for (anotherPerson <- personData){
        if(!map.keys.exists(_==(person, anotherPerson)) && !map.keys.exists(_==(anotherPerson, person))){
          var matchScore = person.bothMatch(anotherPerson)
          var first:Person = person
          var second:Person = anotherPerson
          map += (first, second) -> matchScore
        }

      }
    }
   map
  }
}