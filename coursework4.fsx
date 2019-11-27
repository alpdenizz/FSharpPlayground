(*

  ITT8060 -- Advanced Programming 2017
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Recursive data types

  ------------------------------------
  Name: DENIZALP KAPISIZ
  Student ID: 172658IVSM
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060 under your name, into a file
  coursework4/coursework4.fsx by October 20, 2017.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is
  incorrect it will not be graded.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

*)

// In this coursework you will be required to design a file system data type
// that is able to carry the information about file names and access rights
// (read and/or write).
// The write permission on a directory is required to create or remove files
// in it, but not to write to an existing file in it.
// You will also be asked to create functions that can carry out various tasks
// on a file system.

// The access permissions are given by the following type:

type Permission = Read | Write | ReadWrite

// 0. Define a function
// createEmptyFilesystem: unit -> FileSystem
// that will be a function with 0 arguments that will return an
// empty filesystem of the type that you defined.
// (Permissions are initially assumed to be ReadWrite, check task 5)  
// We assume that your file system is defined in a type called FileSystem.
type FileSystem = Element list
  and Element =
    | Root of FileSystem * Permission
    | File of string * Permission
    | Dir of string * FileSystem * Permission

let createEmptyFilesystem() = [Root ([],ReadWrite)] : FileSystem

// 1. Define two functions 
// createFile : string -> FileSystem -> FileSystem
// that will create a file into the root directory of the 
// current file system.
// The first argument is the file name, the second
// is the filesystem to create the file into. 
// (Permissions are initially assumed to be ReadWrite, check task 5)  

let rec createFile fileName (fileSystem:FileSystem) = 
  match (List.head fileSystem) with
  | Root ([],p) -> [Root ([File (fileName,ReadWrite)],p)]:FileSystem
  | Root (head::tail,p) -> 
    match head with
    | File (name,_) when name=fileName -> failwith "You must choose another name"
    | Dir (name,_,_) when name=fileName -> failwith "You must choose another name"
    | _ -> match List.head (createFile fileName [(Root (tail,p))]) with
            | Root(fS,_) -> [Root ([head]@fS,p)]
            | _ -> createEmptyFilesystem() 
  | _ -> fileSystem

// createDir : string -> FileSystem -> FileSystem
// that will create a directory into the root directory of the current
// file system.
// The second argument is the name of the directory 
// (Permissions are initially assumed to be ReadWrite, check task 5)  
let rec createDir dirName (fileSystem:FileSystem) = 
  match (List.head fileSystem) with
  | Root ([],p) -> [Root ([Dir (dirName, [], ReadWrite)],p)]:FileSystem
  | Root (head::tail,p) -> 
    match head with
    | File (name,_) when name=dirName -> failwith "You must choose another name"
    | Dir (name,_,_) when name=dirName -> failwith "You must choose another name"
    | _ -> match List.head (createDir dirName [(Root (tail,p))]) with
            | Root(fS,_) -> [Root ([head]@fS,p)]
            | _ -> createEmptyFilesystem() 
  | _ -> fileSystem
// 2. Define a function 
// createSubDir : string -> FileSystem -> FileSystem -> FileSystem
// that will create a directory with name given in the first argument and
// contents given as the second argument into a file system given
// as the third argument.
let rec createSubDir dirName (content:FileSystem) (fileSystem:FileSystem) = 
  match (List.head fileSystem) with
  | Root ([],p) -> 
    match (List.head content) with
      | Root([],pe) -> [Root ([Dir (dirName, [], ReadWrite)],p)]:FileSystem
      | Root(fS,pe) -> [Root ([Dir (dirName, fS, ReadWrite)],p)]:FileSystem
      | _ -> fileSystem
  | Root (head::tail,p) -> 
    match head with
    | File (name,_) when name=dirName -> failwith "You must choose another name"
    | Dir (name,_,_) when name=dirName -> failwith "You must choose another name"
    | _ -> match List.head (createSubDir dirName content [(Root (tail,p))]) with
            | Root(fS,_) -> [Root ([head]@fS,p)]
            | _ -> createEmptyFilesystem() 
  | _ -> fileSystem
           

// 3. Define a function
// count : FileSystem -> int
// that will recursively count the number of files in the current filesystem.
let rec count (fileSystem:FileSystem) = 
  match fileSystem with
  | [] -> 0
  | head::tail ->
    match head with
    | Root(fS,_) -> count fS
    | File(_) -> 1 + count tail
    | Dir(_,fS,_) -> (count fS) + (count tail)



// 4. Define a function
// changePermissions Permission -> string list -> FileSystem -> FileSystem
// that will apply the specified permission the file or directory
// represented by a string list. For example, list ["Dir1";"Dir2";"File1"]
// represents a structure where "Dir1" is in the root directory, "Dir2" is
// in "Dir1" and "File1" is in "Dir2".
// to change permission of root directory: changePermissions Permission [] fS fS : root path is []
let rec changePermissions permission path (fileSystem:FileSystem) =
  if List.isEmpty path then 
     match List.head fileSystem with
     | Root (fS,_) -> [Root(fS,permission)]:FileSystem
     | _ -> []
  else
  match path with
  | h::t -> 
    match fileSystem with
    | head::tail -> 
      match head with
      | Root (fSys, p) -> [Root ((changePermissions permission path fSys),p)]:FileSystem 
      | File (fileName, _p) -> if fileName = h then ([File (fileName,permission)] @ tail):FileSystem
                               else ([head] @ changePermissions permission path tail):FileSystem
      | Dir (dirName,fSys,p) -> if dirName = h && (List.isEmpty t) then [Dir (dirName, fSys,permission)] @ tail
                                elif dirName = h && (not (List.isEmpty fSys)) then Dir (dirName, (changePermissions permission t fSys),p) :: tail
                                else [head] @ changePermissions permission path tail
    | [] -> []:FileSystem
  | [] -> fileSystem

// 5. Modify the implementations of createFile and createDir to honor the
// permissions of the current file system level, i.e. if the permission 
// of the current directory is not Write or ReadWrite, the function should fail
// with an exception and an appropriate message (that you should formulate yourself).
// Hint: use the built-in failwith function.

let rec createFile fileName (fileSystem:FileSystem) = 
  match (List.head fileSystem) with
  | Root ([],p) when p<>Read -> [Root ([File (fileName,ReadWrite)],p)]:FileSystem
  | Root (head::tail,p) when p<>Read -> 
    match head with
    | File (name,_) when name=fileName -> failwith "You must choose another name"
    | Dir (name,_,_) when name=fileName -> failwith "You must choose another name"
    | _ -> match List.head (createFile fileName [(Root (tail,p))]) with
            | Root(fS,_) -> [Root ([head]@fS,p)]
            | _ -> createEmptyFilesystem() 
  | Root (_,p) when p=Read -> failwith "You have no permission to create file here"
  | _ -> fileSystem

let rec createDir dirName (fileSystem:FileSystem) = 
  match (List.head fileSystem) with
  | Root ([],p) when p<>Read -> [Root ([Dir (dirName, [], ReadWrite)],p)]:FileSystem
  | Root (head::tail,p) when p<>Read -> 
    match head with
    | File (name,_) when name=dirName -> failwith "You must choose another name"
    | Dir (name,_,_) when name=dirName -> failwith "You must choose another name"
    | _ -> match List.head (createDir dirName [(Root (tail,p))]) with
            | Root(fS,_) -> [Root ([head]@fS,p)]
            | _ -> createEmptyFilesystem() 
  | Root (_,p) when p=Read -> failwith "You have no permission to create directory here"
  | _ -> fileSystem

let rec createSubDir dirName (content:FileSystem) (fileSystem:FileSystem) = 
  match (List.head fileSystem) with
  | Root ([],p) when p<>Read -> 
    match (List.head content) with
      | Root([],pe) -> [Root ([Dir (dirName, [], ReadWrite)],p)]:FileSystem
      | Root(fS,pe) -> [Root ([Dir (dirName, fS, ReadWrite)],p)]:FileSystem
      | _ -> fileSystem
  | Root (head::tail,p) when p<>Read -> 
    match head with
    | File (name,_) when name=dirName -> failwith "You must choose another name"
    | Dir (name,_,_) when name=dirName -> failwith "You must choose another name"
    | _ -> match List.head (createSubDir dirName content [(Root (tail,p))]) with
            | Root(fS,_) -> [Root ([head]@fS,p)]
            | _ -> createEmptyFilesystem() 
  | Root (_,p) when p=Read -> failwith "You have no permission to create sub-directory here"
  | _ -> fileSystem


// 6. Implement the function
// locate : string -> FileSystem -> string list list
// that will locate all files and directories with name matching the first argument
// of the function. The return value should be a list of paths to the files and
// directories. Each path is a list of strings indicating the parent directory
// structure.
// Note that the locate should honor the permissions, i.e. the files from
// directories without a read permission should not be returned.
let rec locate str (fileSystem:FileSystem) =
  match fileSystem with
  | [] -> []: string list list
  | head::tail ->
    match head with
    | Root (fSys,_) -> locate str fSys
    | File (name,permission) -> if name = str && permission<>Write then [[name]] @ locate str tail
                                else locate str tail
    | Dir (name,fSys,permission) -> if name = str && permission<>Write then [[name]] @ List.map (fun x -> [name] @ x ) (locate str fSys) @ locate str tail
                                    else List.map (fun x -> [name] @ x ) (locate str fSys) @ locate str tail

// 7. Implement the function
// delete : string list -> FileSystem -> FileSystem
// which will delete the file or directory specified with the first argument (the path
// represented by the string list) from a file system and return a file system
// not containing the file or directory represented by the first argument.
// If the file or directory does not have a write permission, the deletion should not
// be performed and the original file system should be returned.
let rec delete path (fileSystem:FileSystem) =
  if List.isEmpty path then 
     match List.head fileSystem with
     | Root (_,p) when p<>Read -> [Root([],p)]:FileSystem
     | Root (fSys,p) as root when p=Read -> [root]:FileSystem
     | _ -> fileSystem
  else
  match path with
  | h::t -> 
    match fileSystem with
    | head::tail -> 
      match head with
      | Root (fSys, p) -> [Root ((delete path fSys),p)]:FileSystem 
      | File (fileName, p) when p<>Read -> if fileName = h then (tail):FileSystem
                                           else ([head] @ delete path tail):FileSystem
      | Dir (dirName,fSys,p) -> if dirName = h && (List.isEmpty t) && p<>Read then tail
                                elif dirName = h && (not (List.isEmpty fSys)) then Dir (dirName, (delete t fSys),p) :: tail
                                else [head] @ delete path tail
      | _ -> [head] @ delete path tail
    | [] -> []:FileSystem
  | [] -> fileSystem

// Bonus (1p):
// 8. Implement the function:
// recursiveDelete : string list -> FileSystem ->FileSystem
// that will delete a file or directory given as the first argument from a file
// system specified as the second argument.
// In case the item to be deleted is a directory, it needs to honor permissions
// and recursively only delete files with write permissions from directories with 
// write permissions. Subdirectories which will become empty need to be deleted as well. 
