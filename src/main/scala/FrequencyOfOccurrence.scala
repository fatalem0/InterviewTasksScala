import scala.collection.mutable

object FrequencyOfOccurrence extends App {
  /**
   * Есть строка символов, нужно посчитать частоту появления каждой буквы алфавита в этой строке
   * (прописные и строчные буквы считать одинаковыми)
   */
  def countOccurrence(string: String): mutable.Map[Char, Int] = {
    string.foldLeft(mutable.Map.empty[Char, Int]) { (acc, el) =>
      if (acc.contains(el.toLower)) acc(el.toLower) += 1 else acc(el.toLower) = 1
      acc
    }
  }

  val string = "cabaabbaAbccd"
  println(countOccurrence(string))
}
