package implementation

import scala.io.Source

object main extends App {
  val align = new align()
  val assembleur = new assembleur()
  val t1 = System.nanoTime
  var corona400Reads = Source.fromFile("C://Users//flori//git//bioinfoalignementseq//coronaFragment//MN908947.3_fraction0000500_READS_MIXED.fasta").getLines.toList
  
  corona400Reads= corona400Reads.filterNot(_.contains("read"))
  
  val sequence1 = "CCCTAACACAGCCCTAATCT"
  val sequence14 = "ACCCTAACACAGCCCTAATC"
  val sequence3 = "TACCCTAACACAGCCCTAAT"
  val sequence4 = "ACACTACCCTAACACAGCCC"
  val sequence5 = "CCTAACACAGCCCTAATCTA"
  val sequence6 = "CACTACCCTAACACAGCCCT"
  val sequence7 = "CCTAACACTACCCTAACACA"
  val sequence8 = "TAACACTACCCTAACACAGC"
  val sequence9 = "CTACCCTAACACAGCCCTAA"
  val sequence10 = "CTAACACAGCCCTAATCTAA"
  val sequence11 = "CATCCTAACACTACCCTAAC"
  val sequence12 = "ACTACCCTAACACAGCCCTA"
  val sequence13 = "ATCCTAACACTACCCTAACA"
  val sequence2 = "AACACTACCCTAACACAGCC"
  val sequence15 = "TCCTAACACTACCCTAACAC"
  val sequence16 = "CTAACACTACCCTAACACAG"
  
  val listSeq = List(sequence1,sequence2,sequence3,sequence4,
      sequence5,sequence6,sequence7,sequence8,
      sequence9,sequence10,sequence11,sequence12,
      sequence13,sequence14,sequence15,sequence16)
  
  println("TEST COUNTLIKENESS")    
  println(assembleur.countLikeness(List("A","B","A","B"), List("C","T","C","T"))==0)
  println(assembleur.countLikeness(List("A","B","A","B"), List("A","B","T"))==2)
  println(assembleur.countLikeness(List("A","B","A","B"), List("G","C","G","A","B"))==0)
  println()
  
  println("TEST SHIFT")
  println(assembleur.shift("CTCTCTAB", "CTCTCTAB")==8)
  println(assembleur.shift("ABCTGTCBA", "CT")==0)
  println(assembleur.shift("ABCTGTCBA", "AB")==2)
  println(assembleur.shift("ABCTGTCBA", "T")==0)
  println()
  
  println("TEST GETBESTOVERLAP")
  println(assembleur.getBestOverlap("ACTCTAG","VEUVBEAC")==2)
  println(assembleur.getBestOverlap("ACTCTAG","AGUVBEAT")==2)
  println()
  
  println("TEST ASSEMBLER")
  println(assembleur.assembler("ACTCTAG", "AGTATA"))
  println(assembleur.assembler("AGTATA", "ACTCTAG"))
  println()
  
  println("TEST bestOverLapList")
   println(assembleur.getBestOverlap(sequence1, sequence14))
  println(assembleur.bestOverlapList(sequence1, List(sequence14, sequence3)))
  println(assembleur.bestOverlapList(sequence1, listSeq.filterNot(_.contains(sequence1))))
  println()
  
  println("TEST getBestContig")
  println(assembleur.getBestContig(sequence1, listSeq.filterNot(_.contains(sequence1))))
  println(assembleur.getBestContig(sequence2, listSeq.filterNot(_.contains(sequence2))))
  println(assembleur.getBestContig(sequence6, listSeq.filterNot(_.contains(sequence6))))
  println()
  
  println("TEST ASSEMBLERALL")
  println(assembleur.assemblerAll("", List(sequence1, sequence14, sequence3)))
  println(assembleur.assemblerAll("", listSeq))
  println()
   val duration = (System.nanoTime - t1) / 1e9d
   println(duration)
  //println("Test ultime corona\n")
  //println(assembleur.assemblerAll("",corona400Reads))
 
}