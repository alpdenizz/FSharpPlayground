(*

  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists and tuples, recursion, combination of functions

  ------------------------------------
  Name: DENIZALP KAPISIZ
  TUT Student ID: 172658IVSM
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060 under your name, into a file coursework2/coursework2.fsx by September 29, 2017.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.
*)

// 1. Create a type BibliographyItem that has the following structure:
// string list * string * int * (int * int)
// The meaning of the tuple elements is as follows:
// * The first field represents the list of author names where each name is in the format
//   "Lastname, Firstname1 Firstname2" (i.e. listing all first names after comma)
// * The second field represents the title of the publication
// * The third field represents the year of publication
// * The fourth field represents a pair containing the starting page number and ending page number of the paper.
type BibliographyItem = string list * string * int * (int * int)

// 2. Create a value bibliographyData : BibliographyItem list that contains
// at least 10 different publications on your favourite topic from http://dblp.uni-trier.de/ 
// Please note that you need not read the papers, just pick 10 papers that sound interesting to you from the database.
let bibliographyData : BibliographyItem list = 
  [
   (["K, Divya";"P. M, Amal";"Kumar K. S, Ajish"],"A degree based approach to find Steiner Trees",2016,(383,393));
   (["P. M, Amal"; "Kumar K. S, Ajish"], "An Algorithm for kth Minimum Spanning Tree",2016,(343,354));
   (["Zic, John";"Oakes, Nerolie";"Liu, Dongxi";"Li, Jane";"Wang, Chen";"Chen, Shiping"],"A Secure Integrated Platform for Rapdily Formed Multiorganisation Collaborations",2015,(642,651));
   (["P, Anilal";"Sainandan, B. V.";"Sankara Sai S, Siva";"Yellai, Prabhakara"],"Experimentation and analysis of Multipath TCP",2015,(1,3));
   (["Paal, Beatrix"],"Dynamic Consequences of Stabilization Policies Based on a Return to a Gold Standard",2001,(143,186));
   (["S, Anand";"Bijlani, Kamal";"Suresh, Sheeja";"P, Praphul"],"Attendance Monitoring in Classroom Using Smartphone & Wi-Fi Fingerprinting",2016,(62,67));
   (["Ramesh, Maneesha Vinodini";"P., Prabaharan";"A., Shameem Ansar";"Rekha, P."],"DVM based scalable and adaptive multipath routing in MANET for emergency rescue application",2012,(123,129));
   (["Varghese, Vijo T.";"Sasidhar, Kalyan";"P., Rekha"],"A status quo of WSN systems for agriculture",2015,(1775,1781));
   (["Müller-Staub, Maria";"Paans, Wolter"],"A Standard for Nursing Process - Clinical Decision Support Systems (NP-CDSS)",2016,(810,811));
   (["Kuriakose, Jikku";"P., Vinod"],"Discriminant features for metamorphic malware detection",2014,(406,411));
  ]

// 3. Make a function compareLists : string list -> string list -> int that takes two string lists and
// returns 
// * Less than zero in case the first list precedes the second in the sort order;
// * Zero in case the first list and second list occur at the same position in the sort order;
// * Greater than zero in case the first list follows the second list in the sort order;
// You are encouraged to use String.Compare to compare individual strings. If the first authors are the same
// then the precedence should be determined by the next author.
// A missing author can be considered to be equivalent to an empty string.
// Please note that your implementation should be recursive over the input lists.
let rec compareLists (list1:string list) (list2:string list) =
 match (list1,list2) with
 | [],[] -> 0
 | [],_ -> -1
 | _,[] -> 1
 | (ha::ta),(hb::tb) -> 
  let result = System.String.Compare(ha,hb)
  if result=0 then compareLists ta tb else result;;


// 4. Make a function
// compareAuthors : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors.
// use solution from 3.

let compareAuthors (biblio1:BibliographyItem) (biblio2:BibliographyItem) =
  match (biblio1,biblio2) with
  | (authList1,b:string,c:int,d:int*int),(authList2,e:string,f:int,g:int*int) -> compareLists authList1 authList2;;

// 5. Make a function
// compareAuthorsYears : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors and if the authors are 
// the same then according to years.
let compareAuthorsYears (biblio1: BibliographyItem) (biblio2: BibliographyItem) =
  match (biblio1,biblio2) with
  | (authList1,_,year1:int,_),(authList2,_,year2:int,_) -> 
  let compResult = compareLists authList1 authList2
  if compResult=0 then 
    if (year1 > year2) then 1 
    elif (year1 < year2) then -1
    else 0
  else compResult;; 

// 6. Make a function 
// sortBibliographyByYear : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the year in ascending order
let sortBibliographyByYear (bibliography: BibliographyItem list) =
  let years = List.map(fun(_,_,year,_)->year) bibliography
  let sorted = List.sort(years)
  let uniqSorted = List.ofSeq (set sorted)
  let sortedBiblio = List.map(fun(year)->List.filter(fun(_,_,y,_)-> year=y) bibliography) uniqSorted
  (List.concat sortedBiblio):BibliographyItem list;;

// 7. Make a function 
// sortBibliographyByAuthorYear : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the authors and year in ascending order
let sortBibliographyByAuthorYear (bibliography:BibliographyItem list) = 
  (List.sortWith compareAuthorsYears bibliography):BibliographyItem list;;

// 8. Make a function
// groupByYear : BibliographyItem list -> BibliographyItem list list
// where the return list contains lists of bibliography items published in the same year.
let groupByYear (bibliography: BibliographyItem list) =
  let years = List.map(fun(_,_,year,_)->year) bibliography
  let uniqYears = List.ofSeq (set years)
  (List.map(fun(year)->List.filter(fun(_,_,y,_)-> year=y) bibliography) uniqYears) : BibliographyItem list list
// 9. Make a function
// commaSeparatedList : BibliographyItem list -> string
// That will return a comma separated string representation of the data.
// Use function composition operator "<<" in your implementation. 

let commaSeparatedList (data:BibliographyItem list) = 
  ((fun list -> String.concat "\n" list) << (fun list -> 
      List.map(fun(a,b,c,d)->String.concat ", " ["\""+(string a)+"\""; "\""+(string b)+"\""; string c; "\""+(string d)+"\""]) list)) data;;
