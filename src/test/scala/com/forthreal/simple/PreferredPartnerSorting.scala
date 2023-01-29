package com.forthreal.simple

import org.scalatest.wordspec.AsyncWordSpec

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.Random

class PreferredPartnerSorting extends AsyncWordSpec {
  case class Preference(name: String, preferredPartners: Array[String])

  private val girls = Array("Ann", "Mary", "Lucy", "Nataly", "Kathie", "Dorothy", "Elizabeth")
  private val boys = Array("John", "Oliver", "Andrew", "Alex", "Jeff", "Samuel", "Gregory")

  def listPartners(preferring: Array[String], choices: Array[String]): Array[Preference] = {
    /**
     *
     * @param choiceList input array of names among which choice can be made
     * @param targetArrayOfChosen the 2D array of already chosen names
     * @return next name which is chosen randomly among the names that weren't used at that position in the 2D array
     */
    def getNextChoice(choiceList: Array[String], targetArrayOfChosen: Array[Array[String]]): String = {
      if (choiceList.length >= choices.length)
        throw new RuntimeException("Invalid source array size")
      val availableNames = getChoiceVariationsForPosition(0, choiceList.length, targetArrayOfChosen, Array())
      val availableFromSelected = availableNames.filter{ x => !choiceList.contains(x) }
      if(availableFromSelected.isEmpty) // we've got an unlucky choice to make (the name already exists in the column of targetArray) - redo the whole thing
        null
      else
        availableFromSelected(if(availableFromSelected.length > 1) Random.nextInt(availableFromSelected.length) else 0)
    }

    /**
     *
     * @param searchedName the name for which the check is performed if it was already mentioned at a certain vert position (column) of the 2D array
     * @param chosenPos horizontal position (column) number in the 2D array
     * @param chosenArrayPos internal param (vertical position) used for vertical traversal of the 2D array
     * @param arrayOfChosen the 2D array of chosen names
     * @return if the given name is already used in any other column of the 2D array
     */
    @tailrec
    def nameContainedInChosen(searchedName: String, chosenPos: Int, chosenArrayPos: Int, arrayOfChosen: Array[Array[String]]): Boolean = {
      if(chosenArrayPos < arrayOfChosen.length) {
        if(chosenPos >= arrayOfChosen(chosenArrayPos).length)
          throw new RuntimeException("Invalid array position provided")
        if(arrayOfChosen(chosenArrayPos)(chosenPos).equals(searchedName)) true
        else nameContainedInChosen(searchedName, chosenPos, chosenArrayPos + 1, arrayOfChosen)
      } else false
    }

    /**
     * Returns an array of available names for a certain vertical position, which is detected by traversing the array of provided
     * names and checking each one if it is used in the given column of chosen names
     * @param choicesPos internal variable showing which position in the source array of names is evaluated
     * @param chosenPos horizontal position (column number) in the 2D array of names already chosen
     * @param arrayOfChosen the 2D array of already chosen names
     * @param scanned the internal array which is filled
     * @return the resulting array of choices for the given column number in the 2D array
     */
    @tailrec
    def getChoiceVariationsForPosition(choicesPos: Int, chosenPos: Int, arrayOfChosen: Array[Array[String]], scanned: Array[String]): Array[String] = {
      if(chosenPos >= choices.length)
        throw new RuntimeException("Invalid position in the array of chosen")
      if(choicesPos < choices.length) {
        val currentName = choices(choicesPos)
        val containedInChosen = nameContainedInChosen(currentName, chosenPos, 0, arrayOfChosen)
        val newScanned = if(!containedInChosen) Array.concat(scanned, Array(currentName)) else scanned
        getChoiceVariationsForPosition(choicesPos + 1, chosenPos, arrayOfChosen, newScanned)
      } else scanned
    }

    /**
     * Gets a new array of chosen names with checking for each name in the array that it wasn't mentioned in the respective column
     * of the 2D array
     * @param currentArray internal array being filled
     * @param targetArrayOfChosen the 2D array of names that have been chosen
     * @return a new array of chosen names
     */
    @tailrec
    def getNewArrayOfChosen(currentArray: Array[String], targetArrayOfChosen: Array[Array[String]]): Array[String] = {
      if(currentArray.length < choices.length) {
        val nextChoice = getNextChoice(currentArray, targetArrayOfChosen)
        if(nextChoice != null)
          getNewArrayOfChosen(Array.concat(currentArray, Array(nextChoice)),targetArrayOfChosen)
        else
          getNewArrayOfChosen(Array(),targetArrayOfChosen)
      } else currentArray
    }

    /**
     * Fills up arrayOfChosen with lists of names horizontally
     * @param size the maximum size of the target array
     * @param arrayOfChosen the target array to be filled
     * @return target array populated with values
     */
    @tailrec
    def getList(size: Int, arrayOfChosen: Array[Array[String]]): Array[Array[String]] = {
      if(arrayOfChosen.length < size) {
        val newArray = getNewArrayOfChosen(Array(), arrayOfChosen)
        getList(size, Array.concat(arrayOfChosen, Array(newArray)))
      }
      else arrayOfChosen
    }

    @tailrec
    def makeList(current: Int, choiceList: Array[Array[String]], arrayOfChosen: Array[Preference]): Array[Preference] = {
      if(choiceList.length != preferring.length)
        throw new RuntimeException("Lists of preferring and preferred have distinct sizes")
      if (current >= choiceList.length) arrayOfChosen
      else {
        val preference = Preference(preferring(current), choiceList(current))
        makeList(current + 1, choiceList, Array.concat(arrayOfChosen, Array(preference)))
      }
    }
    makeList(0, getList(preferring.length, Array()), Array())
  }

  "Array of preferences for boys" should {
    "be populated" in {
      Future {
        val preferencesOfBoys = listPartners(boys, girls)
        preferencesOfBoys.foreach { x =>
          println(s"Name: ${x.name}: ${x.preferredPartners.foldLeft("") { (a, b) => s"$a $b" }}")
        }
        assert(preferencesOfBoys.length == boys.length)
      }
    }
  }

  "Array of preferences for girls" should {
    "be populated" in {
      Future {
        val preferencesOfGirls = listPartners(girls, boys)
        preferencesOfGirls.foreach { x =>
          println(s"Name: ${x.name}: ${x.preferredPartners.foldLeft("") { (a, b) => s"$a $b" }}")
        }
        assert(preferencesOfGirls.length == girls.length)
      }
    }
  }
}
