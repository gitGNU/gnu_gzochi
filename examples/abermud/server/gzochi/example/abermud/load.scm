;; load.scm --- AberMUD universe loader implementation

;; Copyright (C) 2012 Julian Graham
;;
;; This software is provided 'as-is', without any express or implied
;; warranty. In no event will the authors be held liable for any damages
;; arising from the use of this software.
;;
;; Permission is granted to anyone to use this software for any purpose,
;; including commercial applications, and to alter it and redistribute it
;; freely.

#!r6rs

;; This module is largely concerned with interpreting the contents of AberMUD
;; universe files (version 9) and using them to bootstrap the object graph for
;; gzochi abermud. It has been tested against the new.uni file included in the
;; gzochi distribution, but may work with other universe files as well.

(library (gzochi example abermud load)
  (export abermud:bootstrap)
  (import (gzochi)
	  (gzochi example abermud data)
	  (rnrs))

  ;; An AberMUD universe file header.

  (define-record-type file-header
    (fields version item-count))

  ;; An item entry in an AberMUD universe file. This data structure uses an 
  ;; index-based reference strategy to represent links to words or other items.
  ;; Like the `abermud:item' type, these records implement a simple 
  ;; polymorphism in the form of one or more "substructures" that are stored as
  ;; a vector in the `properties' field below.
  
  (define-record-type file-item
    (fields name
	    adjective
	    noun
	    state
	    perception
	    next
	    children
	    parent
	    actor-table
	    action-table
	    users
	    class
	    superclass
	    properties
	    
	    object-table
	    subject-table
	    daemon-table))
  
  ;; A row in an AberMUD action table.
  
  (define-record-type file-line
    (fields verb noun1 noun2 data))

  ;; An AberMUD action table.
  
  (define-record-type file-table
    (fields number lines name))
  
  ;; A word definition in an AberMUD universe file.

  (define-record-type file-word
    (fields text code type))

  ;; This is the base class for all AberMUD item substructure types.

  (define-record-type file-substructure
    (fields type))

  ;; The enumeration of possible substructure types.

  (define-enumeration substructure-type 
    (inouthere usertext container userflag userflag2 chain room object player
	       genexit condexit msgexit inherit duped)
    make-substructure-type)

  ;; The player substructure.
  
  (define-record-type file-player
    (parent file-substructure)
    (fields user-key size weight strength flags level score)
    (protocol (lambda (n) 
		(lambda (user-key size weight strength flags level score)
		  (let ((p (n (substructure-type player))))
		    (p user-key size weight strength flags level score))))))
  
  ;; The room substructure.

  (define-record-type file-room
    (parent file-substructure)
    (fields short long flags)
    (protocol (lambda (n)
		(lambda (short long flags)
		  (let ((p (n (substructure-type room))))
		    (p short long flags))))))
  
  ;; The object substructure.

  (define-record-type file-object
    (parent file-substructure)
    (fields text size weight flags)
    (protocol (lambda (n)
		(lambda (text size weight flags)
		  (let ((p (n (substructure-type object))))
		    (p text size weight flags))))))
  
  ;; The general exit substructure.

  (define-record-type file-gen-exit
    (parent file-substructure)
    (fields destinations)
    (protocol (lambda (n)
		(lambda (destinations)
		  (let ((p (n (substructure-type genexit))))
		    (p destinations))))))

  ;; The message exit substructure.

  (define-record-type file-msg-exit
    (parent file-substructure)
    (fields destination text exit-number)
    (protocol (lambda (n)
		(lambda (destination text exit-number)
		  (let ((p (n (substructure-type msgexit))))
		    (p destination text exit-number))))))
  
  ;; The conditional exit substructure. (This substructure type is not 
  ;; supported by gzochi abermud.)

  (define-record-type file-cond-exit
    (parent file-substructure)
    (fields destination table exit-number)
    (protocol (lambda (n)
		(lambda (destination table exit-number)
		  (let ((p (n (substructure-type condexit))))
		    (p destination table exit-number))))))
    
  ;; The container substructure. (This substructure type is not supported by 
  ;; gzochi abermud.)

  (define-record-type file-container
    (parent file-substructure)
    (fields volume flags)
    (protocol (lambda (n)
		(lambda (volume flags)
		  (let ((p (n (substructure-type container))))
		    (p volume flags))))))

  ;; The chained item substructure. (This substructure type is not supported by
  ;; gzochi abermud.)
  
  (define-record-type file-chain
    (parent file-substructure)
    (fields chained)
    (protocol (lambda (n)
		(lambda (chained)
		  (let ((p (n (substructure-type chain))))
		    (p chained))))))

  ;; The user flag array substructure. (This substructure type is not supported
  ;; by gzochi abermud.)
  
  (define-record-type file-user-flag
    (parent file-substructure)
    (fields flags items)
    (protocol (lambda (n)
		(lambda (type flags items)
		  (let ((p (n type)))
		    (p flags items))))))
  
  ;; A substructure for representing duplicated items. (This substructure type
  ;; is not supported by gzochi abermud.)

  (define-record-type file-dup
    (parent file-substructure)
    (fields master)
    (protocol (lambda (n)
		(lambda (master)
		  (let ((p (n (substructure-type duped))))
		    (p master))))))
  
  ;; A substructure for representing item inheritance. (This substructure type
  ;; is not supported by gzochi abermud.)

  (define-record-type file-inherited
    (parent file-substructure)
    (fields master)
    (protocol (lambda (n)
		(lambda (master)
		  (let ((p (n (substructure-type inherit))))
		    (p master))))))
  
  ;; A substructure for items that have custom presence-related descriptive 
  ;; text.

  (define-record-type file-inouthere
    (parent file-substructure)
    (fields in-msg out-msg here-msg)
    (protocol (lambda (n)
		(lambda (in-msg out-msg here-msg)
		  (let ((p (n (substructure-type inouthere))))
		    (p in-msg out-msg here-msg))))))
  
  ;; The user text substructure. (This substructure type is not supported by 
  ;; gzochi abermud.)

  (define-record-type file-user-text
    (parent file-substructure)
    (fields text)
    (protocol (lambda (n)
		(lambda (text)
		  (let ((p (n (substructure-type usertext))))
		    (p text))))))

  ;; Bit flag names.

  (define-record-type file-bit-flag-name
    (fields r-bit o-bit p-bit c-bit))

  ;; The top-level representation of a complete AberMUD universe file. All
  ;; fields are immutable.

  (define-record-type abermud-universe
    (fields header items tables words flag-names class-names bit-flag-names))

  ;; Converts a numeric substructure type identifier to its symbolic 
  ;; representation.

  (define (short->substructure-type s)
    (case s
      ((1) (substructure-type room))
      ((2) (substructure-type object))
      ((3) (substructure-type player))
      ((4) (substructure-type genexit))
      ((5) (substructure-type msgexit))
      ((6) (substructure-type condexit))
      ((7) (substructure-type container))
      ((8) (substructure-type chain))
      ((9) (substructure-type userflag))
      ((14) (substructure-type duped))
      ((16) (substructure-type inouthere))
      ((17) (substructure-type usertext))
      ((18) (substructure-type userflag2))
      ((255) (substructure-type inherit))
      (else (raise (make-assertion-violation)))))
  
  ;; Reads a short integer (16 bits) from the universe input port.

  (define (load-short port)
    (let ((a0 (get-u8 port))
	  (a1 (get-u8 port)))
      (+ (bitwise-arithmetic-shift-left a0 8) a1)))

  ;; Reads a long integer (32 bits) from the universe input port.

  (define (load-long port)
    (let ((a0 (load-short port))
	  (a1 (load-short port)))
      (+ (bitwise-arithmetic-shift-left a0 16) a1)))

  ;; Loads a length-prefixed string of characters from the universe input port.
  ;; Length prefixes greater than the maximum short value signify an empty
  ;; string.
  
  (define (load-string port)
    (let ((len (load-long port)))
      (if (> len 65536)
	  ""
	  (let loop ((c 0) (chars (list)))
	    (if (eqv? c len)
		(list->string (reverse chars))
		(loop (+ c 1) (cons (integer->char (get-u8 port)) chars)))))))
  
  ;; Read the header information from the universe input port: The number of
  ;; items (a long integer), followed by the universe file format version (a
  ;; long integer), followed by two long integers' worth of padding.

  (define (load-header port)
    (let ((item-count (load-long port))
	  (version (load-long port)))
      (load-long port)
      (load-long port)
      
      (make-file-header version item-count)))
  
  ;; Read a presence descriptor substructure.

  (define (load-inouthere port)
    (let ((in-msg (load-string port))
	  (out-msg (load-string port))
	  (here-msg (load-string port)))
      
      (make-file-inouthere in-msg out-msg here-msg)))

  ;; Read a user text substructure.
  
  (define (load-usertext port)
    (let ((text (let loop ((i 0) (text (list)))
		  (if (eqv? i 8) 
		      (reverse text) 
		      (loop (+ i 1) (cons (load-string port) text))))))
      
      (make-file-user-text text)))
  
  ;; Read a container substructure.
  
  (define (load-container port)
    (let ((volume (load-short port))
	  (flags (load-short port)))
      
      (make-file-container volume flags)))
  
  ;; Read a user flags substructure.

  (define (load-userflag type port)
    (let ((flags (let loop ((i 0) (flags (list)))
		   (if (eqv? i 8) 
		       (reverse flags) 
		       (loop (+ i 1) (cons (load-short port) flags)))))
	  (items (let loop ((i 0) (items (list)))
		   (if (eqv? i 8)
		       (reverse items)
		       (loop (+ i 1) (cons (load-item-id port) items))))))
      
      (make-file-user-flag type flags items)))
  
  ;; Read a chained item substructure.

  (define (load-chain port)
    (let ((chained (load-long port)))
      
      (make-file-chain chained)))
  
  ;; Read a room substructure.

  (define (load-room port)
    (let ((short (load-string port))
	  (long (load-string port))
	  (flags (load-short port)))
      
      (make-file-room short long flags)))
  
  ;; Read an object substructure.

  (define (load-object-sub port)
    (let ((t0 (load-string port))
	  (t1 (load-string port))
	  (t2 (load-string port))
	  (t3 (load-string port))
	  (size (load-short port))
	  (weight (load-short port))
	  (flags (load-short port)))
      
      (make-file-object (list t0 t1 t2 t3) size weight flags)))
  
  ;; Read a player substructure.

  (define (load-player port)
    (let ((user-key (load-short port))
	  (size (load-short port))
	  (weight (load-short port))
	  (strength (load-short port))
	  (flags (load-short port))
	  (level (load-short port))
	  (score (load-long port)))
      
      (make-file-player user-key size weight strength flags level score)))
  
  ;; Read a general exit substructure.

  (define (load-gen-exit port)
    (let ((destinations 
	   (let loop ((i 0) (items (list)))
	     (if (eqv? i 12) 
		 (list->vector (reverse items))
		 (loop (+ i 1) (cons (load-item-id port) items))))))
      
      (make-file-gen-exit destinations)))
  
  ;; Read a conditional exit substructure.

  (define (load-cond-exit port)
    (let ((destination (load-item-id port))
	  (table (load-short port))
	  (exit-number (load-short port)))
      
      (make-file-cond-exit destination table exit-number)))
  
  ;; Read a message exit substructure.

  (define (load-msg-exit port)
    (let ((destination (load-item-id port))
	  (text (load-string port))
	  (exit-number (load-short port)))
      
      (make-file-msg-exit destination text exit-number)))
  
  ;; Read an inheritance substructure.
  
  (define (load-inherited port)
    (let ((master (load-item-id port)))
      (make-file-inherited master)))
  
  ;; Read a duplicated item substructure.
  
  (define (load-dup port)
    (let ((master (load-item-id port)))
      (make-file-dup master)))
  
  ;; Read the appropriate substructure from the specified universe file port
  ;; depending on the supplied type.

  (define (load-sub port type)
    (case type
      ((inouthere) (load-inouthere port))
      ((usertext) (load-usertext port))
      ((container) (load-container port))
      ((userflag) (load-userflag type port))
      ((userflag2) (load-userflag type port))
      ((chain) (load-chain port))
      ((room) (load-room port))
      ((object) (load-object-sub port))
      ((player) (load-player port))
      ((genexit) (load-gen-exit port))
      ((condexit) (load-cond-exit port))
      ((msgexit) (load-msg-exit port))
      ((inherit) (load-inherited port))
      ((duped) (load-dup port))
      (else (raise (make-assertion-violation)))))

  ;; Read a reference to a word.
  
  (define (load-word-id port)
    (let ((id (load-short port)))
      (and (not (eqv? id 65535)) id)))

  ;; Read a reference to an item.

  (define (load-item-id port) 
    (let ((id (load-long port))) 
      (and (not (eqv? id 4294967295)) id)))

  ;; Some opaque, hard-coded action table data, swiped from "CondCode.c" in the
  ;; official AberMUD distribution.
  
  (define cond-table
    #("I       AT"
      "I       NOTAT"
      "I       PRESENT"
      "I       ABSENT"
      "I       WORN"
      "I       NOTWORN"
      "I       CARRIED"
      "I       NOTCARR"
      "II      ISAT"
      "II      ISNOTAT"
      "II      ISBY"
      "II      ISNOTBY"
      "F       ZERO"
      "F       NOTZERO"
      "FN      EQ"
      "FN      NOTEQ"
      "FN      GT"
      "FN      LT"
      "FF      EQF"
      "FF      NOTEQF"
      "FF      LTF"
      "FF      GTF"
      "II      ISIN"
      "II      ISNOTIN"
      "a       ADJ1"
      "a       ADJ2"
      "n       NOUN1"
      "n       NOUN2"
      "p       PREP"
      "N       CHANCE"
      "I       ISPLAYER"
      "I       ISUSER"
      "I       ISROOM"
      "I       ISOBJECT"
      "IN      STATE"
      "IP      PFLAG"
      "IO      OFLAG"
      "II      CANPUT"
      "IR      RFLAG"
      "N       LEVEL"
      "        IFDEAF"
      "        IFBLIND"
      "        ARCH"
      "I       GET"
      "I       DROP"
      "I       REMOVE"
      "I       WEAR"
      "I       CREATE"
      "I       DESTROY"
      "I       PUTO"
      "II      SWAP"
      "II      PLACE"
      "II      PUTIN"
      "II      TAKEOUT"
      "IBF     COPYOF"
      "FIB     COPYFO"
      "FF      COPYFF"
      "N       WHATO"
      "NI      GETO"
      "IF      WEIGH"
      "F       SET"
      "F       CLEAR"
      "IP      PSET"
      "IP      PCLEAR"
      "FN      LET"
      "FN      ADD"
      "FN      SUB"
      "FF      ADDF"
      "FF      SUBF"
      "FN      MUL"
      "FN      DIV"
      "FF      MULF"
      "FF      DIVF"
      "FN      MOD"
      "FF      MODF"
      "FN      RANDOM"
      "F       MOVE"
      "I       GOTO"
      "IN      WEIGHT"
      "IN      SIZE"
      "IO      OSET"
      "IO      OCLEAR"
      "IR      RSET"
      "IR      RCLEAR"
      "II      PUTBY"
      "I       INC"
      "I       DEC"
      "IN      SETSTATE"
      "T       PROMPT"
      "F       PRINT"
      "        SCORE"
      "T       MESSAGE"
      "T       MSG"
      "I       LISTOBJ"
      "I       LISTAT"
      "        INVEN"
      "        DESC"
      "T       END"
      "        DONE"
      "        NOTDONE"
      "        OK"
      "        ABORT"
      "        SAVE"
      "T       PARSE"
      "        NEWTEXT"
      "t       PROCESS"
      "ICN     DOCLASS"
      "II      GIVE"
      "INT     SETUT"
      "ITN     DOESACTION"
      "ITIN    DOESTO"
      "ITIN    DOESTOPLAYER"
      "IN      POBJ"
      "IN      PLOC"
      "I       PNAME"
      "I       PCNAME"
      "Ivnn    DAEMON"
      "vnn     ALLDAEMON"
      "Ivnn    HDAEMON"
      "Nt      WHEN"
      "IT      SETNAME"
      "INN     DUP"
      "        FRIG"
      "N       POINTS"
      "N       HURT"
      "N       CURED"
      "T       KILLOFF"
      "v       AUTOVERB"
      "        IF1"
      "        IF2"
      "T       BUG"
      "T       TYPO"
      "FN      NARG"
      "I       ISME"
      "TN      BROADCAST"
      "IT      ISCALLED"
      "II      IS"
      "I       SETME"
      "        PRONOUNS"
      "N       CHANCELEV"
      "I       EXITS"
      "        PWCHANGE"
      "I       SNOOP"
      "NI      UNSNOOP"
      "IN      GETUT"
      "TTT     CAT"
      "T       BECOME"
      "I       ALIAS"
      "        UNALIAS"
      "N       FIELD"
      "N       NEEDFIELD"
      "N       UNVEIL"
      "N       DEBUG"
      "IF      GETSCORE"
      "IF      GETSTR"
      "IF      GETLEV"
      "IF      SETSCORE"
      "IF      SETLEV"
      "IF      SETSTR"
      "T       SHELL"
      "Ic      CSET"
      "Ic      CCLEAR"
      "Ic      CFLAG"
      "I       CANSEE"
      "        RESCAN"
      "vnnN    MEANS"
      "Ivnn    TREEDAEMON"
      "T       SETIN"
      "T       SETOUT"
      "T       SETHERE"
      "IF      CANGOTO"
      "        MOBILES"
      "        DIR"
      "        ROOMS"
      "Ivnn    CHAINDAEMON"
      "IF      CANGOBY"
      "INI     SETIFLAG"
      "INN     GETIFLAG"
      "IN      CLEARIFLAG"
      "II      [FIGHT]"
      "INN     WHERETO"
      "IIF     DOOREXIT"
      "I       CANMOVEROPE"
      "II      PLACEROPE"
      "I       ISROPE"
      "I       ISTIED"
      "IN      ROPEPREV"
      "IN      ROPENEXT"
      "II      TIE"
      "II      UNTIE"
      "II      JOIN"
      "II      CUTROPE"
      "IIN     DISTANCE"
      "IIN     WHICHWAY"
      "IC      CLASSAT"
      "II      DUPOF"
      "IN      MASTEROF"
      "IN      TIEDTO" 
      "$       COMMENT"
      "vanpan  COMVOCAB"
      "vIpI    COMMAND"
      "T       BSXSCENE"
      "NNT     BSXOBJECT"
      "        NOT"
      "        IFDARK"
      "IN      VISIBILITY"
      "IN      GETPARENT"
      "IN      GETNEXT"
      "IN      GETCHILDREN"
      "N       PEXIT"
      "INTTT   SETDESC"
      "ITTT    SETLONG"
      "ITTT    SETSHORT"
      "I       GETLONG"
      "I       GETSHORT"
      "IN      GETDESC"
      "I       GETNAME"
      "        SWAT"
      "F       FLAT"
      "NN      FINDMASTER"
      "INN     NEXTMASTER"
      "INN     FINDIN"
      "INNN    NEXTIN"
      "TN      LENTEXT"
      "I       PROCSUBJECT"
      "I       PROCOBJECT"
      "I       PROCDAEMON"
      "IN      GETSUPER"
      "II      SETSUPER"
      "II      MEMBER"
      "        "
      "        CLS"
      "        "
      "        "
      "        "
      "        "
      "        "
      "        "
      "        "
      "        "
      "        "
      "        "
      "        "
      "IC      ISCLASS"
      "TT      SUBSTR"
      "I       GETIN"
      "I       GETOUT"
      "I       GETHERE"
      "TTT     LOG"
      "IC      SETCLASS"
      "IC      UNSETCLASS"
      "FN      BITCLEAR"
      "FN      BITSET"
      "FN      BITTEST"
      "F       SPRINT"
      "NNNIIT  USER"
      "NI      SETI"
      "Ivnn    CDAEMON"
      "T       DELETE"
      "TI      ULOAD"
      "TI      USAVE"
      "TNN     FLOAD"
      "TNN     FSAVE"
      "IF      GETVIS"
      "IT      MESSAGETO"
      "IT      MSGTO"
      "TTTT    RWHOINIT"
      "        RWHODOWN"
      "TT      RWHOLOGIN"
      "T       RWHOLOGOUT"
      "        RWHOPING"
      "INI     SETEXIT"
      "IN      DELEXIT"
      "INN     GETEXIT"
      "T       FORKDUMP"))

  (define (load-action port offset)
    (define (make-action-pair val)
      (cons (mod val 65536) (/ val 65536)))
  
    (define (load-action-inner c)
      (case c
	((#\$) (case (load-short port)
		 ((0) (make-action-pair 1))
		 ((3) (make-action-pair 3))
		 (else (cons 'comment (load-string port)))))
	((#\T) (case (load-short port)
		 ((0) (make-action-pair 1))
		 ((3) (make-action-pair 3))
		 (else (cons 'string (load-string port)))))
	((#\I) (case (load-short port)
		 ((1) (make-action-pair 1))
		 ((3) (make-action-pair 3))
		 ((5) (make-action-pair 5))
		 ((7) (make-action-pair 7))
		 ((9) (make-action-pair 9))
		 (else (cons 'item (load-item-id port)))))
	((#\B #\C #\c #\R #\O #\P #\F #\v #\p #\n #\a #\t #\N) 
	 (load-short port))
	(else (raise (make-assertion-violation)))))

    (let ((cond-str (vector-ref cond-table offset)))
      (let loop ((i 0) (actions (list)))	
	(let ((c (string-ref cond-str i)))
	  (if (eqv? c #\space)
	      (reverse actions)
	      (loop (+ i 1) (cons (load-action-inner c) actions)))))))

  ;; Read a line from an AberMUD action table.

  (define (load-line port)
    (define *cmd-eol* 10000)
  
    (let ((verb (load-short port))
	  (noun1 (load-short port))
	  (noun2 (load-short port))
	  (data (let loop ((b (load-short port)) (data (list)))
		  (if (eqv? b *cmd-eol*)
		      (reverse data)
		      (let ((actions (load-action port b)))
			(loop (load-short port) (append actions data)))))))
      
      (make-file-line verb noun1 noun2 data)))
  
  ;; Read an AberMUD action table.

  (define (load-table port number)
    (let ((name (load-string port))
	  (lines (let loop ((c (load-short port)) (lines (list)))
		   (if (eqv? c 0)
		       (let ((line (load-line port)))
			 (loop (load-short port) (cons line lines)))
		       (reverse lines)))))
      (make-file-table number lines name)))
  
  ;; Read the action table for an item.

  (define (load-item-table port)
    (and (not (eqv? (load-short port) 0))
	 (begin
	   (load-short port)
	   (load-table port #f))))

  ;; Read an item definition from the specified universe file port.

  (define (load-item port)
    (let ((name (load-string port))
	  (adjective (load-word-id port))
	  (noun (load-word-id port))
	  (state (load-short port))
	  (perception (load-short port))
	  (next (load-item-id port))
	  (children (load-item-id port))
	  (parent (load-item-id port))
	  (actor-table (load-short port))
	  (action-table (load-short port))
	  (users (load-short port))
	  (class (load-short port))
	  (superclass (and (> (load-short port) 0) (load-item-id port))))

      ;; Check to see if the item has substructure properties.

      (if (> (load-long port) 0)
	  (let ((properties 
		 (let loop ((n (load-short port))
			    (properties (list)))
		   (if (eqv? n 0)
		       (list->vector (reverse properties))
		       (let ((p (load-sub
				 port (short->substructure-type n))))
			 (loop (load-short port) (cons p properties))))))
		(object-table (load-item-table port))
		(subject-table (load-item-table port))
		(daemon-table (load-item-table port)))
	    
	    (make-file-item 
	     name adjective noun state perception next children parent 
	     actor-table action-table users class superclass properties 
	     object-table subject-table daemon-table))
	  
	  (make-file-item 
	   name adjective noun state perception next children parent 
	   actor-table action-table users class superclass #f #f #f #f))))

  ;; Read a word definition.

  (define (load-word port)
    (define (read-token)
      (define (read-token-inner lst)
	(let ((c (integer->char (get-u8 port))))
	  (if (or (eqv? c #\space) (eqv? c #\.))
	      (list->string (reverse lst))
	      (read-token-inner (cons c lst)))))

      (read-token-inner (list)))

    (let ((text (read-token))
	  (code (string->number (read-token)))
	  (type (string->number (read-token))))

      (if (equal? text ";END") #f (make-file-word text code type))))

  ;; Read a flag name definition.

  (define (load-flag-name port)
    (define (load-flag-name-inner i lst)
      (if (eqv? i 0)
	  (list->string (reverse lst))
	  (load-flag-name-inner 
	   (- i 1) (cons (integer->char (get-u8 port)) lst))))

    (let ((len (load-short port)))
      (and (not (eqv? len 0)) (load-flag-name-inner len '()))))
    
  ;; Read a class name, or `#f' if it is prefixed by a short-encoded zero.

  (define (load-class-name port)
    (and (not (eqv? (load-short port) 0))
	 (load-string port)))

  ;; Read a bit flag name definition.

  (define (load-bit-flag-name port)
    (define (load-buf)
      (define (load-buf-inner lst)
	(let ((c (get-u8 port)))
	  (if (eqv? c 1) 
	      (list->string lst)
	      (load-buf-inner (cons (integer->char c) lst)))))

      (load-buf-inner '()))

    (make-file-bit-flag-name (load-buf) (load-buf) (load-buf) (load-buf)))

  ;; Reads the contents of an AberMUD universe file from the specified port and
  ;; constructs and returns an in-memory representation of the universe. The
  ;; structure of the universe file is, in general terms:
  ;; 
  ;; 1. Header (version number, item and word counts)
  ;; 2. Item definitions
  ;; 3. Action tables
  ;; 4. Word definitions
  ;; 5. Flag names
  ;; 6. Class names
  ;; 7. Bit flag names

  (define (load-system port)
    (let* ((header (load-header port))

	   ;; Read the number of items specified in the header.
	   
	   (items (let loop ((c 0) (items (list)))
		    (if (eqv? c (file-header-item-count header))
			(list->vector (reverse items))
			(loop (+ c 1) (cons (load-item port) items)))))

	   ;; Read action table data until a short-format zero delimiter is
	   ;; encountered.

	   (tables (let loop ((c (load-short port)) (tables (list)))
		     (if (not (eqv? c 0))
			 (reverse tables)
			 (let ((table (load-table port (load-short port))))
			   (loop (load-short port) (cons table tables))))))

	   ;; Read words until a null word is encountered.

	   (words (let loop ((words (list)))
		    (let ((word (load-word port)))
		      (if word 
			  (loop (cons word words)) 
			  (list->vector (reverse words))))))

	   ;; Read 512 flag names.

	   (flag-names
	    (let ((flag-names (make-vector 512)))
	      (let loop ((i 0))
		(if (eqv? i 512) 
		    flag-names 
		    (begin (vector-set! flag-names i (load-flag-name port))
			   (loop (+ i 1)))))))

	   ;; Read 16 class names.
	   
	   (class-names
	    (let ((class-names (make-vector 16)))
	      (let loop ((i 0))
		(if (eqv? i 16)
		    class-names
		    (begin (vector-set! class-names i (load-class-name port))
			   (loop (+ i 1)))))))

	   ;; Read 16 bit flag names.

	   (bit-flag-names
	    (let ((bit-flag-names (make-vector 16)))
	      (let loop ((i 0))
		(if (eqv? i 16)
		    bit-flag-names
		    (begin (vector-set! 
			    bit-flag-names i (load-bit-flag-name port))
			   (loop (+ i 1))))))))
      
      (make-abermud-universe 
       header items tables words flag-names class-names bit-flag-names)))

  ;; The next series of functions is concerned with marshalling between the 
  ;; data structures used to hold the file contents of the AberMUD universe
  ;; file and the managed types that are used by gzochi abermud.

  ;; Converts a symbolic gzochi abermud word type to an AberMUD file word type
  ;; id.

  (define (abermud-word-type->file-word-type type)
    (case type
      ((abermud:word-type noun) 1)
      ((abermud:word-type verb) 5)
      ((abermud:word-type adjective) 6)
      (else (raise (make-assertion-violation)))))

  ;; Converts an AberMUD word definition structure to an `abermud:word' record.

  (define (file-word->abermud-word file-word)
    (define (file-word-type->abermud-word-type type)
      (case type
	((1) (abermud:word-type noun))	
	((5) (abermud:word-type verb))
	((6) (abermud:word-type adjective))
	(else #f)))
    (let ((text (file-word-text file-word))
	  (type (file-word-type->abermud-word-type 
		 (file-word-type file-word))))
      (and type (abermud:make-word text type))))

  ;; Returns the gzochi abermud facet type (as a managed record type 
  ;; descriptor) to use to represent the specified AberMUD substructure type,
  ;; or `#f' if the substructure type is not supported.

  (define (file-substructure-type->abermud-facet-type type)
    (case type
      ((room genexit msgexit) abermud:room)
      ((inouthere) abermud:inouthere)
      ((object) abermud:object)
      (else #f)))

  ;; The supported object flag bits.

  (define file-object-flag-flannel 1)
  (define file-object-flag-can-get 16)
  
  ;; Returns a (potentially empty) list of symbols corresponding to supported
  ;; AberMUD object flags (namely, "flannel" and "can get") from the specified
  ;; object substructure.

  (define (file-object-flags->abermud-object-flags substructure)
    (let* ((flags (list))
	   (flags (if (positive? 
		       (bitwise-and 
			file-object-flag-flannel 
			(file-object-flags substructure)))
		      (cons (abermud:object-flag flannel) flags)
		      flags))
	   (flags (if (positive?
		       (bitwise-and 
			file-object-flag-can-get
			(file-object-flags substructure)))
		      (cons (abermud:object-flag can-get) flags)
		      flags)))
      flags))

  ;; Constructs and returns a gzochi abermud facet that corresponds to the
  ;; specified AberMUD substructure, or `#f' if the substructure type is not
  ;; supported.

  (define (file-substructure->abermud-facet substructure)
    (case (file-substructure-type substructure)
      ((inouthere)
       (abermud:make-inouthere 
	(file-inouthere-in-msg substructure)
	(file-inouthere-out-msg substructure)
	(file-inouthere-here-msg substructure)))
      ((room) 
       (abermud:make-room
	(file-room-short substructure) 
	(file-room-long substructure)))
      ((object)
       (abermud:make-object 
	(car (file-object-text substructure))
	(file-object-flags->abermud-object-flags substructure)))
      (else #f)))

  ;; Searches the specified word vector for words matching the specified code
  ;; and word type.

  (define (find-file-word file-words code type)
    (let ((num-file-words (vector-length file-words)))
      (let loop ((i 0))
	(and (< i num-file-words)
	     (let ((word (vector-ref file-words i)))
	       (if (and (eqv? (file-word-code word) code)
			(eqv? (file-word-type word) type))
		   word (loop (+ i 1))))))))

  ;; Constructs and returns a gzochi abermud item record that corresponds to
  ;; the specified AberMUD item. Supported substructures that are part of the
  ;; AberMUD item will converted to AberMUD facets of the appropriate type.
  ;; `abermud:word' entries that correspond to the text and type of any words
  ;; associated with the item will be looked up and attached to the resulting
  ;; `abermud:item' record.

  (define (file-item->abermud-item file-item universe)
    (define file-words (abermud-universe-words universe))

    (let* (;; Locate the item's adjective, if any, in the registry of words 
	   ;; from the specified universe.

	   (file-adjective (file-item-adjective file-item))
	   (file-adjective 
	    (and file-adjective 
		 (find-file-word 
		  file-words file-adjective 
		  (abermud-word-type->file-word-type 
		   (abermud:word-type adjective)))))

	   ;; Resolve the adjective in the registry of gzochi abermud words.

	   (abermud-adjective
	    (and file-adjective
		 (abermud:find-word
		  (file-word-text file-adjective) 
		  (abermud:word-type adjective))))

	   ;; Locate the item's noun, if any, in the registry of words from the
	   ;; specified universe.

	   (file-noun (file-item-noun file-item))
	   (file-noun
	    (and file-noun
		 (find-file-word 
		  file-words file-noun	
		  (abermud-word-type->file-word-type 
		   (abermud:word-type noun)))))

	   ;; Resolve the noun in the registry of gzochi abermud words.

	   (abermud-noun 
	    (and file-noun
		 (abermud:find-word
		  (file-word-text file-noun) (abermud:word-type noun))))

	   (item (abermud:make-item abermud-adjective abermud-noun)))
      
      (vector-for-each 
       (lambda (property)
	 (let ((facet (file-substructure->abermud-facet property))) 
	   (and facet (abermud:add-facet! item facet))))
       (file-item-properties file-item))
      item))

  ;; Converts an exit index from an AberMUD general exit substructure to its
  ;; symbolic representation in the `abermud:direction' enumeration.

  (define (file-exit-index->abermud:direction file-exit-index)
    (case file-exit-index
      ((0) (abermud:direction north))
      ((1) (abermud:direction east))
      ((2) (abermud:direction south))
      ((3) (abermud:direction west))
      ((4) (abermud:direction up))
      ((5) (abermud:direction down))
      ((6) (abermud:direction northeast))
      ((7) (abermud:direction southeast))
      ((8) (abermud:direction northwest))
      ((9) (abermud:direction southwest))
      ((10) (abermud:direction in))
      ((11) (abermud:direction out))
      (else (raise (make-assertion-violation)))))

  ;; Performs a reference resolution pass over a new `abermud:item' record's
  ;; facet ensuring that its connectivity with other managed objects mirrors 
  ;; that of the specified AberMUD item substructure.

  (define (link-facet abermud-facet file-facet managed-items)

    ;; Links an AberMUD message exit subsructure by adding its message and
    ;; destination to an `abermud:item' room facet.

    (define (link-message-exit abermud-room file-msg-exit managed-items)
      (abermud:add-room-exit!
       abermud-room
       (file-exit-index->abermud:direction 
	(file-msg-exit-exit-number file-msg-exit))
       (vector-ref managed-items (file-msg-exit-destination file-msg-exit))
       (file-msg-exit-text file-msg-exit)))

    ;; Links an AberMUD general exit subsructure by adding its destinations
    ;; (with blank exit messages) to an `abermud:item' room facet.

    (define (link-general-exit abermud-room file-gen-exit managed-items)
      (let ((file-destinations (file-gen-exit-destinations file-gen-exit)))
	(let loop ((i 0))
	  (or (eqv? i 12)
	      (let ((index (vector-ref file-destinations i)))
		(if index 
		    (abermud:add-room-exit!
		     abermud-room 
		     (file-exit-index->abermud:direction i) 
		     (vector-ref managed-items index)))
		(loop (+ i 1)))))))

    ;; If the gzochi abermud item facet is a room and the AberMUD universe
    ;; substructure is a general exit or a message exit, link the gzochi
    ;; abermud item's exit destinations appropriately.

    (if (abermud:room? abermud-facet)
	(cond ((file-gen-exit? file-facet)
	       (link-general-exit abermud-facet file-facet managed-items))
	      ((file-msg-exit? file-facet)
	       (link-message-exit 
		abermud-facet file-facet managed-items)))))
 
  ;; Performs general and facet-specific "linking" for a new gzochi abermud
  ;; item: Essentially, resolving AberMUD universe item number references to
  ;; other newly-created `abermud:item' records.

  (define (link-item abermud-item file-item managed-items file-items)

    ;; Add a reference to the parent item, if any.

    (let ((parent-id (file-item-parent file-item)))
      (and parent-id
	   (abermud:set-item-parent! 
	    abermud-item (vector-ref managed-items parent-id))))

    ;; Add references to the child items, if any.

    (let loop ((child-id (file-item-children file-item)))
      (if child-id
	  (let ((child-file-item (vector-ref file-items child-id))
		(child-managed-item (vector-ref managed-items child-id)))
	    (abermud:link-item! abermud-item child-managed-item)
	    (loop (file-item-next child-file-item)))))

    ;; Perform per-substructure-type linking (to handle things like room 
    ;; exits): Loop over all substructures in the corresponding AberMUD
    ;; universe item, and match them to a facet on the `abermud:item' record.

    (let* ((file-properties (file-item-properties file-item))
	   (num-properties (vector-length file-properties)))
      (let loop ((i 0))
	(or (eqv? i num-properties)
	    (let* ((file-substructure (vector-ref file-properties i))
		   (facet-type (file-substructure-type->abermud-facet-type 
				(file-substructure-type file-substructure))))
	      
	      (if facet-type
		  (link-facet (abermud:as-facet abermud-item facet-type)
			      file-substructure
			      managed-items))
	      (loop (+ i 1)))))))

  ;; Bootstraps a gzochi abermud game world using the contents of the specified
  ;; AberMUD universe file input port. All items and words are loaded from the
  ;; universe, converted to the appropriate gzochi abermud data structures, and
  ;; stored in managed hashtables with global bindings in the application
  ;; context.

  (define (abermud:bootstrap port)

    ;; Load the contents of the AberMUD universe file into a detached, 
    ;; unmanaged graph of records.

    (define universe (load-system port))

    ;; Convert all words from the universe to `abermud:word' records and
    ;; insert them into the word table.

    (vector-for-each (lambda (file-word)
		       (let ((word (file-word->abermud-word file-word))) 
			 (if word (abermud:add-word! word))))
		     (abermud-universe-words universe))

    ;; Convert all items from the universe to `abermud:item' records and
    ;; insert them into the item table.
    
    (let ((managed-items
	   (vector-map
	    (lambda (file-item)
	      (let ((item (file-item->abermud-item file-item universe)))
		(if (abermud:item-noun item)
		    (if (abermud:item-adjective item)
			(abermud:add-item! item 
					   (abermud:item-adjective item) 
					   (abermud:item-noun item))
			(abermud:add-item! item (abermud:item-noun item))))
		item))
	    (abermud-universe-items universe))))
    
      ;; Resolve all links between items (parent-child or facet-based) by
      ;; locating the `abermud:item' instances associated with AberMUD items by
      ;; their item numbers.

      (let ((file-items (abermud-universe-items universe)))
	(vector-for-each 
	 (lambda (managed-item file-item)
	   (link-item managed-item file-item managed-items file-items))
	 managed-items file-items))))
)
