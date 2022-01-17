package implementation

import java.security.Key
import scala.collection.immutable.ListMap

class align {
  
  def convertList(l : List[String]) : List[List[String]] = l match {
    case Nil => Nil
    case hd::tl => hd.split("").toList::convertList(tl)
  }
  
  def assemble2brin(seq1 : List[String], seq2 : List[String]) : String =(seq1,seq2) match {
    case (a,Nil) =>a.mkString("") 
    case (Nil,b) => b.mkString("")
    case _ => seq1.dropRight(1).equals(seq2.drop(1)) match {
      case false => seq2.dropRight(1).equals(seq1.drop(1)) match {
        case true => seq1.head+seq2.mkString("")
        case false => "pas marché"
        }
      case true => seq2.head+seq1.mkString("")
      }
  }
  
  def assembleAll(listSeq :  List[List[String]]) : String = listSeq match {
    case Nil => ""
    case hd::md::Nil => assemble2brin(hd,md)
    case hd::md::tl => assemble2brin(hd,md)+assembleAll(md::tl)
  }
  
  def multiplieEach(l : List[String]) : List[String] = l match {
    case Nil => Nil
    case hd::tl => hd++hd::multiplieEach(tl)
  }
  
  def findAnchor(listSeq : List[String]) : String = listSeq.head.dropRight(36) match {
    case anchor => listSeq.filter(_.contains(anchor)) match {
      case list => if (list.length==listSeq.length) anchor
      else "J'ai pas d'ancre"
    }
  }
  
  def placeOfAnchor(listSeq : List[String], anchor : String ) : Map[String,Int] ={
    listSeq match {
      case Nil => Map()
      case hd::tl=> Map(hd->hd.indexOf(anchor))++placeOfAnchor(tl,anchor)
    }
  }
  
  def putInOrder(map : Map[String,Int]) : Map[String,Int] = {
     val res = ListMap(map.toSeq.sortWith(_._2 < _._2):_*)
     res
  }
  
  /**
   * seq2 a le meme debut que seq1 sans le premier element
   * @return boolean
   */
  def startWith(seq1 : String, seq2 : String) : Boolean = seq2.take(4)==seq1.tail.take(4)
  
  def deleteLastChar(listSeq : List[String]) : List[String] = listSeq match {
    case Nil => Nil
    case hd::tl => hd.dropRight(1)::deleteLastChar(tl)
  }
  
  def findPair(seq1 : String , listSeq : List[String]) : Option[String] = {
    val seqFound = listSeq.find(x => {deleteLastChar(listSeq).contains(seq1.tail)})
    seqFound match {
      case None => None
      case Some(a) => assemblage(seq1,a) match {
        case Some(b) =>if(listSeq==Nil) Some(b)
        else findPair(b,listSeq.filterNot(x => x.contains(seq1) && x.contains(a)))
        case None => None
      }
    }
  }
  
  def assemblage(seq1 : String, seq2 : String) : Option[String] = startWith(seq1,seq2) match {
    case true => Some(seq1.head+seq2)
    case false => startWith(seq2,seq1) match {
      case true => Some(seq2.head+seq1)
      case false => None
    }
  }
}