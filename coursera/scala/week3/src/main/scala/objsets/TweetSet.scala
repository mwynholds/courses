package objsets

import common._
import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    user + " '" + text + "' (" + retweets + ")"
  def contains(keyword: String): Boolean = text.contains(keyword)
  def containsAny(keywords: List[String]): Boolean = {
    if (keywords.isEmpty) return false
    contains(keywords.head) || containsAny(keywords.tail)
  }
}

abstract class TweetSet {

  def isEmpty: Boolean
  def filter(p: Tweet => Boolean): TweetSet = this.filterAcc(p, new Empty)
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet
  def union(that: TweetSet): TweetSet
  def head: Tweet
  def tail: TweetSet
  def mostRetweeted: Tweet
  def descendingByRetweet: TweetList
  def incl(tweet: Tweet): TweetSet
  def remove(tweet: Tweet): TweetSet
  def contains(tweet: Tweet): Boolean
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def isEmpty: Boolean = true
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc
  def union(that: TweetSet): TweetSet = that
  def head: Tweet = throw new Error("cannot get head of empty set")
  def tail: TweetSet = throw new Error("cannot get tail of empty set")
  def contains(tweet: Tweet): Boolean = false
  def mostRetweeted: Tweet = null
  def descendingByRetweet: TweetList = Nil
  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)
  def remove(tweet: Tweet): TweetSet = this
  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def isEmpty: Boolean = false

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val children = left.filterAcc(p, acc).union(right.filterAcc(p, acc))
    if (p(elem)) children.incl(elem)
    else children
  }

  def union(that: TweetSet): TweetSet = {
    if (that.isEmpty) this
    else this.incl(that.head).union(that.tail)
  }

  def head: Tweet = if (left.isEmpty) elem else left.head

  def tail: TweetSet = remove(head)

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def max2(t1: Tweet, t2: Tweet): Tweet = {
    if (t1 == null && t2 == null) return null
    if (t1 == null)               return t2
    if (t2 == null)               return t1
    if (t1.retweets > t2.retweets) t1
    else t2
  }

  def max(t1: Tweet, t2: Tweet, t3: Tweet): Tweet =
    max2(max2(t1, t2), t3)

  def mostRetweeted: Tweet = {
    max(elem, left.mostRetweeted, right.mostRetweeted)
  }

  def descendingByRetweet: TweetList = {
    val tweet = mostRetweeted
    new Cons(tweet, remove(tweet).descendingByRetweet)
  }

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
  override def toString = "Nil"
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
  override def toString = head.user + ", " + tail
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = findTweets(google)
  lazy val appleTweets: TweetSet = findTweets(apple)

  def findTweets(keywords: List[String]): TweetSet = {
    val all = TweetReader.allTweets
    all.filter( (t: Tweet) => t.containsAny(keywords) )
  }

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  println("Running GoogleVsApple...")
  GoogleVsApple.trending foreach println
}
