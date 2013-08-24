package com.twitter.t02uk.artsetouchi

import nu.validator.htmlparser.sax.HtmlParser
import nu.validator.htmlparser.common.XmlViolationPolicy
import scala.xml.NodeSeq
import scala.io.Source
import scala.xml.parsing.NoBindingFactoryAdapter
import java.io.InputStream
import org.xml.sax.InputSource
import java.io.StringReader
import java.io.File
import java.util.Calendar
import java.io.PrintWriter
import java.text.SimpleDateFormat

object Analyzer {
  val acceptVenues = List("南寺", "地中美術館", "豊島美術館")

  private[this] def now = new SimpleDateFormat().format(Calendar.getInstance().getTime())

  private[this] lazy val hp = {
    val hp = new HtmlParser
    hp.setNamePolicy(XmlViolationPolicy.ALLOW)
    hp
  }

  /**
   * A tiny NodeSeq wrapper for filter attribute of NodeSeq without dull process
   */
  implicit class NodeSeqExt(self: NodeSeq) {
    type Selector = (NodeSeq, String) => NodeSeq
    // 
    def has(selector: Selector)(attribute: String)(value: String): NodeSeq = {
      self filter (x => selector(x, "@" + attribute) flatMap(_.toString.split("""\s""")) contains value)
    }
    // x \ y
    def yen1Quote: Selector = ((x, y) => x \ y)
    // .class selector
    def \* = has(yen1Quote)("class") _
    // #hoge selector
    def \# = has(yen1Quote)("id") _
    // name = "name"
    def named = has(yen1Quote)("name") _
  }
  
  def main(args: Array[String]) {
    
    val logs = new File("./log").listFiles().toList
    val parsed = logs.flatMap { log =>
      val re = """(\d{4}-\d{2}-\d{2}-\d{2}-\d{2})\.html""".r

      val dateTime = log.getName() match {
        case re(dateTime) => DateTime(dateTime)
      }
      val source = Source.fromFile(log).mkString
      parseNews(source, dateTime)
    }
    
    val availableDates = parsed.map(_.gotDateTime.date).distinct.sorted
    val displayDates = availableDates
    val displayTime = (9 to 19).map(x => DateTime(0, 0, 0, x, 0, 0).hhmm).sorted
    val htmls = acceptVenues.map { venue =>
       val onlyOnThisPlace = parsed.filter(_.venue == venue)
       val groupByDateTime = onlyOnThisPlace.groupBy(x => (x.gotDateTime.date, x.gotDateTime.hhmm))
       val default = groupByDateTime.head._2.head
       val rows = displayDates.map { date =>
         (date -> displayTime.map { hhmm =>
           lazy val x = Jam(DateTime(date, hhmm), "", default.island, default.venue, "", false, "")
           groupByDateTime.get(date, hhmm).map(_.head).getOrElse(x)
         })
       }
       
       val formatedRows = rows.map { x =>
         val (date, row) = x
         
         val formated = row.sortBy(_.gotDateTime.hhmm.toString).map { col =>
           col match {
             case Jam(_, _, _, _, _, true, _) => <td class="soldout" width="64px" align="center" style="background:#fcc">完売</td>
             case Jam(_, _, _, _, t, _, _) if !t.isEmpty => <td width="64px" align="right">{t}</td>
             case Jam(_, _, _, _, _, _, _) => <td width="64px">&nbsp;</td>
           }
         }
         
         val re = """.+\((.+)\).*""".r
         val background = date match {
           case re("Sat") => "background:#ccf;"
           case re("Sun") => "background:#fcc;"
           case _ => "background:#cfc;"
         }
<tr>
  <td class="date" style={background}>{date}</td>
  { for(x <- formated) yield x }
</tr>
       }
       
       val title = displayTime.map(x => <td style="text-align:center;background:#cfc">{x}</td>)

<table border="1">
  <tbody>{venue}</tbody>
  <td> </td> {title}
  { for(x <- formatedRows) yield x }
</table>
<hr />
    }
    
val content = <html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
</head>
<body>
<h1 style="font-size: 120%">瀬戸内国際芸術祭の混雑状況 (Genrated at { now } )</h1>
{htmls}
<p><a href="https://github.com/t02uk/analyze_artsetouchi">Source Codes (github)</a></p>
</body>
</html>

    val p = new PrintWriter("./analyzed_artsetouchi.html")
    p.print(content)
    p.flush()
    p.close()
    
    println("complete!")
  }
  
  def parseNews(source: String, gotDateTime: DateTime): Seq[Jam] = {

    val saxer = new NoBindingFactoryAdapter
    hp.setContentHandler(saxer)
    hp.parse(new InputSource(new StringReader(source)))

    val news = saxer.rootElem \ "body" \ "div" \ "div" \ "div" \# "primary" \ "div" \ "div" \ "table" \* "livenewstable" \ "tbody" \ "tr"
    
    news.map { tr =>
      val columns = tr \\ "td"
      val time = columns(0)
      val island = columns(1)
      val description = columns(2)
      val venue = description \ "b"
      
      val descriptionText = description.text.replaceAll("""\s+""", " ");
      val jam = analyzeDescription(venue.text, zen2han(descriptionText))
      
      Jam(
        gotDateTime,
        time.text.trim(),
        island.text.trim(),
        venue.text.trim(),
        jam.map(_._1).getOrElse(""),
        jam.map(_._2).getOrElse(false),
        descriptionText
      )
    }
  }
  
  def zen2han(src: String): String = {
    val han = "0123456789".toSeq
    val zen = "０１２３４５６７８９".toSeq
    
    han.zip(zen).foldLeft(src) { (acc, hanzen)  =>
      val (han, zen) = hanzen
      acc.replaceAll(zen.toString, han.toString)
    }
  }
  
  def analyzeDescription(venue: String, description: String): Option[(String, Boolean)] = {
    
    val reWait1 = ".+入館(.+)待ち.+".r
    val reWait2 = ".+待ち時間(.+)程度.+".r
    val reWait3 = ".+現在(.+)待ち.+".r
    val reNoWait1 = ".+待ち時間(?:なく|はありません).+".r
    val reSoldOut1 = ".+本日分完売.+".r
    val reSoldOut2 = ".+配布は?終了.+".r

    venue match {
      case venue if acceptVenues.contains(venue) =>
        description.replaceAll("""\r\n|\r|\n""", "") match {
          case reNoWait1() => Some("", false)
          case reSoldOut1() => Some("", true)
          case reSoldOut2() => Some("", true)
          case reWait1(hm) => Some(formatWaitingTime(hm), false)
          case reWait2(hm) => Some(formatWaitingTime(hm), false)
          case reWait3(hm) => Some(formatWaitingTime(hm), false)
          case _ => None
        }
      case _ => None
    }
  }
  
  def formatWaitingTime(string: String): String = {
    val reHM = """(\d+)時間(\d+)分""".r
    val reH = """(\d+)時間""".r
    val reM = """(\d+)分""".r
    
    val x = string match {
      case reHM(h, m) => (h.toInt, m.toInt)
      case reH(h) => (h.toInt, 0)
      case reM(m) => (0, m.toInt)
    }
    
    "%2d:%02d".format(x._1 + x._2 / 60, x._2 % 60)
      
  }
}