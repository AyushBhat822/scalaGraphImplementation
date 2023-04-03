package learningGraph
import collection.mutable.*

object WordLadderProblemI extends App{

  class Pair(val first: String, val second: Int)

  class Solution {
    def wordLadderLength(startWord: String, targetWord: String, wordList: Array[String]): Int = {
      // Creating a queue ds of type {word,transitions to reach ‘word’}.
      val q = scala.collection.mutable.Queue[(String, Int)]()

      // BFS traversal with pushing values in queue
      // when after a transformation, a word is found in wordList.
      q.enqueue((startWord, 1))

      // Push all values of wordList into a set
      // to make deletion from it easier and in less time complexity.
      var st = scala.collection.mutable.Set[String]()
      st ++= wordList
      st -= startWord

      while (q.nonEmpty) {
        val (word, steps) = q.dequeue()

        // We return the steps as soon as the first occurrence of targetWord is found.
        if (word == targetWord) return steps

        // Now, replace each character of ‘word’ with char from a-z then check if ‘word’ exists in wordList.
        for (i <- 0 until word.length()) {
          for (ch <- 'a' to 'z') {
            val replacedWord = word.updated(i, ch)

            // check if it exists in the set and push it in the queue.
            if (st.contains(replacedWord)) {
              st -= replacedWord
              q.enqueue((replacedWord, steps + 1))
            }
          }
        }
      }

      // If there is no transformation sequence possible.
      0
    }
  }
    val startWord = "der"
    val targetWord = "dfs"
    val wordList = Array("des", "der", "dfr", "dgt", "dfs")
    val obj = new Solution
    val ans = obj.wordLadderLength(startWord, targetWord, wordList)
    println(ans)

}
