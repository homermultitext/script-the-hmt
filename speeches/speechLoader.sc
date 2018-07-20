
import scala.io.Source
import edu.holycross.shot.cite._

case class Speaker (urn: Cite2Urn, label: String, gender: String)

case class Speech (speech: Cite2Urn, speaker: Speaker, passage: CtsUrn)


val nameList = "data/speakersList.cex"
val speechList = "data/speeches.cex"

def loadSpeakers(fileName: String) = {
  val speakerList = Source.fromFile(fileName).getLines.toVector.drop(1)
  val speakers = for (n <- speakerList) yield {
    val cols = n.split("#")
    val urn = cols(0)
    val nameLabel = cols(1)
    val gender = cols(2)
    Speaker(Cite2Urn(urn), nameLabel, gender)
  }
  speakers
}
val speakers = loadSpeakers(nameList)


// read a file of speech data, and create
// a vector of Option[Speech] obejcts
def speechOpts(fileName: String) = {
  val speechData = Source.fromFile(fileName).getLines.toVector

  //Speech#Speaker#Passage
  val speechOpts = for (sp <- speechData.tail) yield {
    val cols = sp.split("#")
    try {
      val speechUrn = Cite2Urn(cols(0))
      val speakerUrn = Cite2Urn(cols(1))
      val psg = CtsUrn(cols(2))
      val speakerMatches = speakers.filter(_.urn == speakerUrn)

      if (speakerMatches.isEmpty) {
        println("Data syntactically good, but failed to match a speaker on " + speakerUrn)
        println(cols.toVector)

        None
      } else {
        //println("Speaker " + speakerMatches(0))
        Some(Speech(speechUrn, speakerMatches(0) ,psg))
      }


    } catch {
      case t: Throwable => {
        println("Failed on " + cols.toVector)
        println(t + "\n")
        None
      }
    }
  }

  println("\n\n")
  println("Speech data read from  " + speechData.size + " lines of data.")
  println("No speaker assigned for " + speechOpts.filter(_ == None).size + " data lines.")
  speechOpts
}
// load a vector of Speech objects from a afile
def loadSpeeches(fileName: String) = {
  speechOpts(fileName).flatten
}
val speeches = loadSpeeches(speechList)
val distinctSpeakers = speeches.map(_.speaker).distinct
// Speakers appearing in speakers' list,
// with assignment of



def reportForId(talker : Cite2Urn): Unit = {

  val speakerInfo = speakers.filter(_.urn == talker)
  val speakingTimes = speeches.filter(_.speaker.urn == talker)

  println("\n" + speakerInfo(0).label + " speaks " + speakingTimes.size + " times.")
  println(" in .... lines")
  println(" in .... books")

}


def speakerReport(label: String) : Unit = {
  val speakerRecords = speakers.filter(_.label.toLowerCase.contains(label.toLowerCase))
  speakerRecords.size match {
    case 0 => println("No labels for speakers matched '" + label + "'.")
    case 1 => reportForId( speakerRecords(0).urn )
    case _ => {
      println("More than one record matched '" + label + "' ")
      for (recrd <- speakerRecords) {
        println("\t" + recrd.label)
      }
    }

  }
}






println("Total speeches: " + speeches.size )
/*
println("\nNumber of speakers: " + distinctSpeakers.size)
println("\tmale: " + distinctSpeakers.filter(_.gender == "m").size)
println("\tfemale: " + distinctSpeakers.filter(_.gender == "f").size)


println("\n\nTo see information on an individual speaker:")
println("\n\tspeakerReport(\"LABEL\")")
println("\nwhere LABEL is (part of) a labelling string,e.g.")
println("\n\tspeakerReport(\"Achilles\")")
*/


/*n
val speakingFolks = speeches.map(_.speaker).distinct



val grouped  = speeches.groupBy(_.speaker)
val numSpeechesGrouped = grouped.map{ case (k,v) => (k, v.size) }
val numSpeechesSorted = numSpeechesGrouped.sortBy(_._2).reverse

val numSpeechesSortedCex = numSpeechesSorted.map{ case (sp, count) => s"${sp.urn}#${sp.label}#${count}"}

import java.io.PrintWriter
new PrintWriter("speechesByPerson.txt"){write(numSpeechesSortedCex.mkString("\n") +"\n"); close;}


import edu.holycross.shot.ohco2._

val f = "data/hmt-2018f-texts.cex"
val corpus = TextRepositorySource.fromCexFile(f).corpus

// count of lines per speech
val counts = for (s <- speeches) yield {
  val speechLines = corpus ~~ s.passage
  println("For " + s + ",  found " + speechLines.size + " matches.")
  (speechLines.size, s)
}
val countsCex = for (c <- counts) yield {
  s"${c._2.speech}#${c._2.speaker.urn}#${c._2.speaker.label}#${c._2.passage}#${c._1}" }
new PrintWriter ("linesPerSpeech.cex"){write(countsCex.mkString("\n") + "\n"); close;}


// cluster line counts by speaker
val bySpeaker = counts.map( c => (c._2.speaker, c._1) ).groupBy(_._1)

val speakerNumbers = bySpeaker.map{ case (k,v) => k -> v.map( spCount => spCount._2).sum }

val byBook = counts.map( c => (CtsUrn(c._2.passage.dropPassage + c._2.passage.rangeBegin).collapsePassageTo(1), c._1))
*/
