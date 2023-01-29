package com.forthreal.simple

import org.scalatest.wordspec.AsyncWordSpec

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.Random

/**
You are given an N by M 2D matrix of lowercase letters. Determine the minimum number of columns that can be removed to ensure that each row is ordered from top to bottom lexicographically. That is, the letter at each column is lexicographically later as you go down each row. It does not matter whether each row itself is ordered lexicographically.

For example, given the following table:

cba
daf
ghi
This is not ordered because of the a in the center. We can remove the second column to make it ordered:

ca
df
gi
So your function should return 1, since we only needed to remove 1 column.

As another example, given the following table:

abcdef
Your function should return 0, since the rows are already ordered (there's only one row).

As another example, given the following table:

zyx
wvu
tsr
Your function should return 3, since we would need to remove all the columns to order it.
 */

class CaseLexicographicOrdering extends AsyncWordSpec {
  private val matrixSizeX = 5
  private val matrixSizeY = 3
  private val letters = "abcdefghijklmnopqrstvuwxyz"

  private def fillRow(): String = {
    @tailrec
    def fillRow(resultingRow: String): String = {
      if (resultingRow.length < matrixSizeY)
        fillRow(s"$resultingRow${letters(Random.nextInt(letters.length))}")
      else resultingRow
    }
    fillRow("")
  }

  private def makeLine(): Array[String] = {
    @tailrec
    def makeLine(array: Array[String]): Array[String] =
      if(array.length < matrixSizeX) makeLine(Array.concat(array,Array(fillRow)))
      else array.array
    makeLine(Array())
  }

  private def `isLineOrdered?`(line: String): Boolean = {
    @tailrec
    def `isLineOrdered?`(line: String, nextSym: Int): String =
      if(nextSym >= line.length) line
      else if(nextSym == 0 || line(nextSym - 1) < line(nextSym))
        `isLineOrdered?`(line, nextSym + 1)
      else line.substring(0, nextSym - 1)

    `isLineOrdered?`(line, 0).length == line.length
  }

  "ResultingRow " should {
    "be of correct size" in {
      Future {
        val calc = Array(0)
        val resultingMatrix = makeLine
        resultingMatrix.foreach( a => if(!`isLineOrdered?`(a)) calc(0) = calc(0) + 1 )
        assert(calc(0) > 0)
      }
    }
  }

}
