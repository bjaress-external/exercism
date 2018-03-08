object Bob {

  def response(statement: String): String =
    if (isShouting(statement) && isQuestion(statement))
      "Calm down, I know what I'm doing!"
    else if (isShouting(statement))
      "Whoa, chill out!"
    else if (isQuestion(statement))
      "Sure."
    else if (isEmpty(statement))
      "Fine. Be that way!"
    else
      "Whatever."

  def isQuestion(statement: String): Boolean =
    statement.reverse.dropWhile(_.isWhitespace).startsWith("?")

  def isShouting(statement: String): Boolean =
    !statement.exists(_.isLower) && statement.exists(_.isUpper)

  def isEmpty(statement: String): Boolean = statement.forall(_.isWhitespace)
}
