package implementation

class assembleur {
  
  def bestOverlapList(contig : String, listContig : List[String]) : Int ={
    listContig match {
      case Nil => 0
      case hd::Nil => getBestOverlap(contig, hd)
      case hd::tl => Math.max(getBestOverlap(contig, hd),bestOverlapList(contig, tl))
    }
  }
  
  def getBestOverlap(contig1 : String, contig2 : String) : Int  = {
    contig1.contains(contig2) match {
      case true => contig2.length
      case false => contig2.contains(contig1) match {
        case true => contig1.length
        case false => Math.max(shift(contig1,contig2),shift(contig1.reverse,contig2.reverse))
      }
    }
  }

  def shift(contig1 : String, contig2 : String) : Int = {
    contig1.startsWith(contig2 take contig2.length/2) match {
      case true => countLikeness(contig1.split("").toList, contig2.split("").toList)
      case false => if(contig2.length()<=0) 0
      else shift(contig1,contig2 tail)
    }
  }
  
  def countLikeness(contig1 : List[String] , contig2 : List[String] ) : Int ={
    (contig1,contig2) match {
      case (Nil,Nil) => 0
      case (Nil, list) => 0
      case (list,Nil) => 0
      case (hd::tl, hd2::tl2) => if(hd==hd2) 1+countLikeness(tl,tl2)
      else 0
    }
  }
    
  def getBestContig(contig : String, listContig : List[String]) : String ={
    listContig match {
      case Nil => contig
      case hd::tl => getBestOverlap(contig, hd)==bestOverlapList(contig,listContig) match {
        case true => hd
        case false => getBestContig(contig, tl)
      }
    }
  }
  
  
  def assembler(contig1 : String, contig2 : String) : String ={
    val overlap = getBestOverlap(contig1,contig2)
    contig1 contains contig2 match {
      case true => contig1
      case false =>  contig2 contains contig1 match {
        case true => contig2
        case false => contig1.endsWith(contig2.slice(0,overlap)) match {
          case true => contig1+contig2.drop(overlap)
          case false => contig2+contig1.drop(overlap)
        }
      }
    }
  }
  
  def assemblerAll(contig1 : String, listContig : List[String]) : String = {
    print("debug ")
    (contig1,listContig) match {
      case (contig,Nil) => contig1
      case ("",list) => assemblerAll(list head, list tail)
      case (contig,list) => val listMoins1 = listContig.filterNot(_.contains(contig))
        assemblerAll(
          assembler(contig,getBestContig(contig, listMoins1)),
          list filterNot(_ contains getBestContig(contig, listMoins1)) filterNot(_ contains contig))
    }
  }
}