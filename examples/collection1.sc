
import edu.holycross.shot.cite._

val speeches = Vector(
  "urn:cite2:hmt:speech.v1:1#urn:cite2:hmt:pers.v1:pers44#urn:cts:greekLit:tlg0012.tlg001.msA:1.17-1.21",
  "urn:cite2:hmt:speech.v1:43#urn:cite2:hmt:pers.v1:pers22#urn:cts:greekLit:tlg0012.tlg001.msA:1.26-1.32",
  "urn:cite2:hmt:speech.v1:44#urn:cite2:hmt:pers.v1:pers44#urn:cts:greekLit:tlg0012.tlg001.msA:1.37-1.42",
  "urn:cite2:hmt:speech.v1:45#urn:cite2:hmt:pers.v1:pers1#urn:cts:greekLit:tlg0012.tlg001.msA:1.59-1.67"
)


















/*
for (speech <- speeches) {
  val columns = speech.split("#")
  val speechId = Cite2Urn(columns(0))
  val person = Cite2Urn(columns(1))
  val passage = CtsUrn(columns(2))
  println("Person: " + person.objectComponent)
}
*/
