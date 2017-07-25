package objsets

object TweetReader {

  object ParseTweets {
    import scala.util.parsing.json._
    
    def getList[T](s: String): List[T] =
      JSON.parseFull(s).get.asInstanceOf[List[T]]

    def getMap(s: String): Map[String, Any] =
      JSON.parseFull(s).get.asInstanceOf[Map[String, Any]]

    def getTweets(user: String, json: String): List[Tweet] =
      for (map <- getList[Map[String, Any]](json)) yield {
        val text = map("text")
        val retweets = map("retweet_count")
        new Tweet(user, text.toString, retweets.toString.toDouble.toInt)
      }

    def getTweetData(user: String, json: String): List[Tweet] = {
      // is list
      val l = getList[Map[String, Any]](json)
      for (map <- l) yield {
        val text = map("text")
        val retweets = map("retweets")
        new Tweet(user, text.toString, retweets.toString.toDouble.toInt)
      }
    }
  }

  def toTweetSet(l: List[Tweet]): TweetSet = {
    l.foldLeft(new Empty: TweetSet)(_.incl(_))
  }

  def unparseToData(tws: List[Tweet]): String = {
    val buf = new StringBuffer
    for (tw <- tws) {
      val json = "{ \"user\": \"" + tw.user + "\", \"text\": \"" +
                                    tw.text.replaceAll(""""""", "\\\\\\\"") + "\", \"retweets\": " +
                                    tw.retweets + ".0 }"
      buf.append(json + ",\n")
    }
    buf.toString
  }

  val sites = List("gizmodo", "TechCrunch", "engadget", "amazondeals", "CNET", "gadgetlab", "mashable")
  
  val gizmodoTweets = TweetReader.ParseTweets.getTweetData("gizmodo", TweetData.gizmodo)
  val techCrunchTweets = TweetReader.ParseTweets.getTweetData("TechCrunch", TweetData.TechCrunch)
  val engadgetTweets = TweetReader.ParseTweets.getTweetData("engadget", TweetData.engadget)
  val amazondealsTweets = TweetReader.ParseTweets.getTweetData("amazondeals", TweetData.amazondeals)
  val cnetTweets = TweetReader.ParseTweets.getTweetData("CNET", TweetData.CNET)
  val gadgetlabTweets = TweetReader.ParseTweets.getTweetData("gadgetlab", TweetData.gadgetlab)
  val mashableTweets = TweetReader.ParseTweets.getTweetData("mashable", TweetData.mashable)
  
  private val sources = List(gizmodoTweets, techCrunchTweets, engadgetTweets, amazondealsTweets, cnetTweets, gadgetlabTweets, mashableTweets)

  val tweetMap: Map[String, List[Tweet]] =
    Map() ++ Seq((sites(0) -> gizmodoTweets),
                 (sites(1) -> techCrunchTweets),
                 (sites(2) -> engadgetTweets),
                 (sites(3) -> amazondealsTweets),
                 (sites(4) -> cnetTweets),
                 (sites(5) -> gadgetlabTweets),
                 (sites(6) -> mashableTweets))

  val tweetSets: List[TweetSet] = sources.map(tweets => toTweetSet(tweets))
  
  private val siteTweetSetMap: Map[String, TweetSet] =
    Map() ++ (sites zip tweetSets)

  private def unionOfAllTweetSets(curSets: List[TweetSet], acc: TweetSet): TweetSet =
    if (curSets.isEmpty) acc
    else unionOfAllTweetSets(curSets.tail, acc.union(curSets.head))

  val allTweets: TweetSet = unionOfAllTweetSets(tweetSets, new Empty)
}
