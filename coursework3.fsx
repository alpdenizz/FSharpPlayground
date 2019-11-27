(*

  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: Discriminated unions, higher order functions

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
  repository itt8060 under your name, into a file coursework3/coursework3.fsx by October 11, 2017.
  NB! The deadline has been extended!
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.
*)

// 1. Create a type BibliographyItem data structure based on discriminated unions such that it supports data
// for bibliography items for
// * article (journal paper)
// * inproceedings (conference paper)
// * book
// * MSc thesis
// * Web page (misc)
// Use the specifications given at http://newton.ex.ac.uk/tex/pack/bibtex/btxdoc/node6.html
// You should support all mandatory and optional fields of each entry. The names of the fields should
// be the same as in the referenced web page. You should capitalize the names of constructors in 
// discriminated unions.
type ArticleData =
  {
    author: string list
    title: string
    journal: string
    year: int
    volume: int option 
    number: int option 
    pages: (int*int) option
    month: int option
    note: string option;
  }

type InproceedingsData =
  {
    author: string list
    title: string
    booktitle: string 
    year: int 
    editor: string option list
    volume: int option 
    number: int option
    series: string option
    pages: (int*int) option
    address: string option
    month: int option 
    organization: string option
    publisher: string option
    note: string option;
  }

type BookData =
  {
    author: string option list
    editor: string option list
    title: string
    publisher: string
    year: int
    volume: int option 
    number: int option
    series: int option
    address: string option
    edition: string option
    month: int option
    note: string option;
  }
 
 type MscThesisData =
  {
    author: string list
    title: string
    school: string list
    year: string
    type_: string option
    address: string option
    month: int option
    note: string option;
  }

 type MiscData =
  {
    author: string option list 
    title: string option 
    howpublished: string option
    month: int option
    year: int option
    note: string option;
  }

type BibliographyItem =
| Article of ArticleData
| Inproceedings of InproceedingsData
| Book of BookData
| MscThesis of MscThesisData
| Misc of MiscData

// 2. Create a value bibliographyData : BibliographyItem list that contains
// at least 10 different publications on your favourite topic from http://dblp.uni-trier.de/ 
// and the MSc thesis databases of UT and TTÜ. At least one instance of every publication needs to be 
// Please note that you need not read the papers, just pick 10 papers that sound interesting to you from the database.

let bibliographyData = 
  [
    Article {author=["Julian Jamison"]; 
             title="Games with Synergistic Preferences"; 
             journal="Games"; 
             year=2012; 
             volume=Some 3; 
             number=Some 1; 
             pages=Some (41,55); 
             month=Some 3; 
             note=None
             };
    Article {author=["Justin Keen"]; 
             title="Digital health care: Cementing centralisation?"; 
             journal="Health Informatics Journal"; 
             year=2014; 
             volume=Some 20; 
             number=Some 3; 
             pages=Some (168,175); 
             month=Some 9; 
             note=None
             };
    Inproceedings {
    author = ["Volker Roth"];
    title = "Empowering Mobile Software Agents";
    booktitle = "Mobile Agents";
    year = 2002;
    editor = [Some "Niranjan Suri"];
    volume = Some 2535; 
    number = None;
    series = Some "LNCS";
    pages = Some (47,63);
    address = Some "Barcelona, Spain";
    month = Some 10;
    organization = None;
    publisher = Some "Springer, Berlin, Heidelberg";
    note = None
    };
    Inproceedings {
    author = ["Julia L. Lawall"];
    title = "Implementing Circularity Using Partial Evaluation";
    booktitle = "Programs as Data Objects";
    year = 2001;
    editor = [Some "Olivier Danvy"; Some "Andrzej Filinski"];
    volume = Some 2053; 
    number = None;
    series = Some "LNCS";
    pages = Some (84,102);
    address = Some "Aarhus, Denmark";
    month = Some 5;
    organization = None;
    publisher = Some "Springer, Berlin, Heidelberg";
    note = None
    };
    Book {
    author = [Some "Andrew S. Tanenbaum"; Some "David Wetherall"];
    editor = [None];
    title = "Computer Networks";
    publisher = "Pearson";
    year = 2011;
    volume = None; 
    number = None;
    series = None;
    address = Some "Upper Saddle River, N.J. and Harlow";
    edition = Some "(5th. ed.)";
    month = None;
    note =  None
    };
    Book {
    author = [Some "Jure Leskovec";Some "Anand Rajaraman";Some "Jeffrey D. Ullman"];
    editor = [None];
    title = "Mining of Massive Datasets";
    publisher = "Cambridge University Press";
    year = 2014;
    volume = None; 
    number = None;
    series = None;
    address = Some "Cambridge University";
    edition = Some "(2nd. ed.)";
    month = None;
    note =  None
    };
    MscThesis {
    author = ["Foto Afrati";"Jeffrey Ullman"];
    title = "Matching Bounds for the All-pairs MapReduce Problem";
    school = ["National Technical University of Athens"; "Stanford University"];
    year = string 2013;
    type_ = None;
    address = Some "New York, NY, USA";
    month = Some 10;
    note = None
    };
    MscThesis {
    author = ["Jeffrey D.Ullman"];
    title = "Designing Good MapReduce Algorithms";
    school = ["Stanford University"];
    year = string 2012;
    type_ = None;
    address = Some "New York, NY, USA";
    month = Some 9;
    note = None
    };
    Misc {
    author = [Some "Harry Thornburg"];
    title = Some "Introduction to Bayesian Statistics";
    howpublished = Some "Retrieved from http://ccrma.stanford.edu/~jos/bayes/bayes.html";
    month = Some 3;
    year = Some 2001;
    note = None
    };
    Misc {
    author = [Some "Wayne W. LaMorte"];
    title = Some "Central Limit Theorem";
    howpublished = Some "Retrieved from http://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_probability/BS704_Probability12.html";
    month = Some 7;
    year = Some 2016;
    note = None
    }
  ]

// 3. Create a function formatInACMReferenceStyle : BibliographyItem -> string that will format the bibliography items
// using the reference style specified here: http://www.acm.org/publications/authors/reference-formatting
let tupleConvert (a,b) =
  if (a=0 && b=0) then ""
  else (string a)+"-"+(string b)

let issueData month year sign =
  match month with
  | 1 -> "("+"January "+(string year)+")"+sign
  | 2 -> "("+"February "+(string year)+")"+sign
  | 3 -> "("+"March "+(string year)+")"+sign
  | 4 -> "("+"April "+(string year)+")"+sign
  | 5 -> "("+"May "+(string year)+")"+sign
  | 6 -> "("+"June "+(string year)+")"+sign
  | 7 -> "("+"July "+(string year)+")"+sign
  | 8 -> "("+"August "+(string year)+")"+sign
  | 9 -> "("+"September "+(string year)+")"+sign
  | 10 -> "("+"October "+(string year)+")"+sign
  | 11 -> "("+"November "+(string year)+")"+sign
  | 12 -> "("+"December "+(string year)+")"+sign
  | _ -> ""

let dealEmpty value sign =
  if value=0 then ""
  else (string value)+sign

let dealEmpty2 value sign =
  if value="" then ""
  else value+sign  


let formatInACMReferenceStyle bibliographyItem =
  match bibliographyItem with
  | Article article -> String.concat ". " [(String.concat ", " article.author); (string article.year); 
      article.title; (article.journal+" "+(dealEmpty (defaultArg article.volume 0) ", ")+(dealEmpty (defaultArg article.number 0) " ")+(issueData (defaultArg article.month 0) article.year ", ")+(tupleConvert (defaultArg article.pages (0,0))) ) ]
  | Book book -> (String.concat ", " (List.map (fun x -> defaultArg x "") book.author)) + ". " + (string book.year) + ". " + book.title + ", " + (dealEmpty (defaultArg book.volume 0) ", ")+(dealEmpty (defaultArg book.number 0) ", ")+ (dealEmpty (defaultArg book.series 0) ". ")+ (dealEmpty2 (defaultArg book.edition "") ". ") + (issueData (defaultArg book.month 0) book.year ". ") + book.publisher + ", " + (defaultArg book.address "")    
  | Inproceedings inproceeding ->  (String.concat ", " inproceeding.author) + ". " + (string inproceeding.year) + ". " + inproceeding.title + ". " + inproceeding.booktitle + ", "+(dealEmpty (defaultArg inproceeding.volume 0) ", ")+(dealEmpty (defaultArg inproceeding.number 0) ", ")+(dealEmpty2 (defaultArg inproceeding.series "") ". ")+(issueData (defaultArg inproceeding.month 0) inproceeding.year ". ") + (dealEmpty2 (defaultArg inproceeding.organization "") ". ") + (dealEmpty2 (defaultArg inproceeding.publisher "") ", ") + (dealEmpty2 (defaultArg inproceeding.address "") ", ") + (tupleConvert(defaultArg inproceeding.pages (0,0)))
  | MscThesis thesis -> (String.concat ", " thesis.author) + ". " + (string thesis.year) + ". " + thesis.title + ". " + (issueData (defaultArg thesis.month 0) (int thesis.year)  ". ") + "Master's thesis" + ". " + (String.concat ", " thesis.school)
  | Misc misc -> (String.concat ", " (List.map (fun x -> defaultArg x "") misc.author)) + ". " + (dealEmpty (defaultArg misc.year 0) ". ") + (dealEmpty2 (defaultArg misc.title "") ". ") + (issueData (defaultArg misc.month 0) (defaultArg misc.year 0) ". ") + (defaultArg misc.howpublished "")

// 4. Write a function compareByAuthorYear : BibliographyItem -> BibliographyItem -> int that will compare the authors and year of the
// bibliography item in the same way as specified in coursework2.
let rec compareLists (list1:string list) (list2:string list) =
 match (list1,list2) with
 | [],[] -> 0
 | [],_ -> -1
 | _,[] -> 1
 | (ha::ta),(hb::tb) -> 
  let result = System.String.Compare(ha,hb)
  if result=0 then compareLists ta tb else result;;

let myCompare author1 year1 author2 year2 =   
    if compareLists author1 author2 = 0 then 
      if (year1 > year2) then 1 
      elif (year1 < year2) then -1
      else 0
    else compareLists author1 author2

let compareByAuthorYear (bibItem1:BibliographyItem) (bibItem2:BibliographyItem) =
  match bibItem1,bibItem2 with
  | Article article1 , Article article2 -> myCompare article1.author article1.year article2.author article2.year
  | Article article, Inproceedings inproc -> myCompare article.author article.year inproc.author inproc.year
  | Article article, Book book -> myCompare article.author article.year (List.map (fun x -> defaultArg x "") book.author) book.year
  | Article article, MscThesis thesis -> myCompare article.author article.year thesis.author (int thesis.year)
  | Inproceedings inproc, Article article -> myCompare inproc.author inproc.year article.author article.year
  | Inproceedings inproc1, Inproceedings inproc2 -> myCompare inproc1.author inproc1.year inproc2.author inproc2.year
  | Inproceedings inproc, Book book -> myCompare inproc.author inproc.year (List.map (fun x -> defaultArg x "") book.author) book.year
  | Inproceedings inproc, MscThesis thesis -> myCompare inproc.author inproc.year thesis.author (int thesis.year)
  | Book book,Article article -> myCompare (List.map (fun x -> defaultArg x "") book.author) book.year article.author article.year
  | Book book,Inproceedings inproc -> myCompare (List.map (fun x -> defaultArg x "") book.author) book.year inproc.author inproc.year
  | Book book1,Book book2 -> myCompare (List.map (fun x -> defaultArg x "") book1.author) book1.year (List.map (fun x -> defaultArg x "") book2.author) book2.year
  | Book book,MscThesis thesis -> myCompare (List.map (fun x -> defaultArg x "") book.author) book.year thesis.author (int thesis.year)
  | MscThesis thesis,Article article -> myCompare thesis.author (int thesis.year) article.author article.year
  | MscThesis thesis,Inproceedings inproc -> myCompare thesis.author (int thesis.year) inproc.author inproc.year
  | MscThesis thesis,Book book -> myCompare thesis.author (int thesis.year) (List.map (fun x -> defaultArg x "") book.author) book.year
  | MscThesis thesis1,MscThesis thesis2 -> myCompare thesis1.author (int thesis1.year) thesis2.author (int thesis2.year)
  | Article article, Misc misc -> myCompare article.author article.year (List.map (fun x -> defaultArg x "") misc.author) (defaultArg misc.year 0)
  | Inproceedings inproc, Misc misc -> myCompare inproc.author inproc.year (List.map (fun x -> defaultArg x "") misc.author) (defaultArg misc.year 0)
  | Book book, Misc misc -> myCompare (List.map (fun x -> defaultArg x "") book.author) book.year (List.map (fun x -> defaultArg x "") misc.author) (defaultArg misc.year 0)
  | MscThesis thesis, Misc misc -> myCompare thesis.author (int thesis.year) (List.map (fun x -> defaultArg x "") misc.author) (defaultArg misc.year 0)
  | Misc misc,Article article -> myCompare (List.map (fun x -> defaultArg x "") misc.author) (defaultArg misc.year 0) article.author article.year
  | Misc misc,Inproceedings inproc -> myCompare (List.map (fun x -> defaultArg x "") misc.author) (defaultArg misc.year 0) inproc.author inproc.year
  | Misc misc,Book book -> myCompare (List.map (fun x -> defaultArg x "") misc.author) (defaultArg misc.year 0) (List.map (fun x -> defaultArg x "") book.author) book.year
  | Misc misc, MscThesis thesis -> myCompare (List.map (fun x -> defaultArg x "") misc.author) (defaultArg misc.year 0) thesis.author (int thesis.year)
  | Misc misc1, Misc misc2 -> myCompare (List.map (fun x -> defaultArg x "") misc1.author) (defaultArg misc1.year 0) (List.map (fun x -> defaultArg x "") misc2.author) (defaultArg misc2.year 0)

// 5. Write a function orderBibliography: (BibliographyItem -> BibliographyItem -> int) -> BibliographyItem list -> BibliographyItem list
// That will order the list of bibliography items according to the given comparison function. 
let orderBibliography (f: BibliographyItem -> BibliographyItem -> int) bibliographyData = List.sortWith f bibliographyData

// 6. Write a function formatBibliographyItems : (BibliographyItem -> string) -> BibliographyItem list -> string list that will take
// a formatting function and a bibliography list and produce a string list that contains formatted bibliography.
let formatBibliographyItems (f: BibliographyItem -> string) bibliographyData = List.map f bibliographyData

// 7. Write a function getNumberedBibliography : BibliographyItem list -> string
// that contains a numbered bibliography where each bibliography item is preceded with a sequence number surrounded
// by square brackets [] and ends with a newline character '\n'.
// The implementation should involve List.fold or List.foldBack function, whichever you deem appropriate.
let getNumberedBibliography bibliographyData = 
  let stringList = formatBibliographyItems formatInACMReferenceStyle bibliographyData
  let indexedList = List.indexed stringList
  let sequencedList = List.map (fun (i,str) -> (string [i+1])+" "+str) indexedList
  List.fold (fun acc x -> acc+"\n"+x) "" sequencedList

// 8. Create 5 appropriate functions to create BibliographyItem data instances. Please note that 
// it is up to you to define the internal data structure. The following functions will be used for generating data in your
// format.
(* 
createArticle :
  author:string list ->
    title:string ->
      journal:string ->
        year:int ->
          volume:int option ->
            number:int option ->
              (int * int) option ->
                month:int option ->
                  note:string option -> BibliographyItem
*)
let createArticle author title journal year volume number pages month note = 
  Article {
    author=author; 
    title=title; 
    journal=journal; 
    year=year; 
    volume=volume; 
    number=number; 
    pages=pages; 
    month=month; 
    note=note
  };;
(*
createBook :
  author:string option list ->
    editor:string option list ->
      title:string ->
        publisher:string ->
          year:int ->
            volume:int option ->
              number:int option ->
                series:int option ->
                  address:string option ->
                    edition:string option ->
                      month:int option ->
                        note:string option -> BibliographyItem
*)
let createBook author editor title publisher year volume number series address edition month note =
  Book {
    author = author;
    editor = editor;
    title = title;
    publisher = publisher;
    year = year;
    volume = volume; 
    number = number;
    series = series;
    address = address;
    edition = edition;
    month = month;
    note =  note
  };;
(*
createInProceedings :
  author:string list->
    title:string ->
      booktitle:string ->
        year:int ->
          editor:string option list ->
            volume:int option ->
              number:int option ->
                series:string option ->
                  (int * int) option ->
                    address:string option ->
                      month:int option ->
                        organization:string option ->
                          publisher:string option ->
                            note:string option -> BibliographyItem
*)
let createInProceedings author title booktitle year editor volume number series pages address month organization publisher note =
  Inproceedings {
    author = author;
    title = title;
    booktitle = booktitle;
    year = year;
    editor = editor;
    volume = volume; 
    number = number;
    series = series;
    pages = pages;
    address = address;
    month = month;
    organization = organization;
    publisher = publisher;
    note = note
  };;
(*
createMScThesis :
  author:string list ->
    title:string ->
      school:string ->
        year:string ->
          type_:string option ->
            address:string option ->
              month:int option ->
                note:string option -> BibliographyItem

*)
let createMScThesis author title school year type_ address month note  = 
  MscThesis {
    author = author;
    title = title;
    school = school;
    year = year;
    type_ = type_;
    address = address;
    month = month;
    note = note
  };;
(*
createMisc :
  author:string option list ->
    title:string option ->
      howpublished:string option ->
        month:int option ->
          year:int option ->
            note:string option -> BibliographyItem

*)
let createMisc author title howpublished month year note =
  Misc {
    author = author;
    title = title;
    howpublished = howpublished;
    month = month;
    year = year;
    note = note
  };;