(**************************************************************************)
(*  PPrint                                                                *)
(*                                                                        *)
(*  Authors: François Pottier and Nicolas Pouillard,                      *)
(*           INRIA Paris-Rocquencourt                                     *)
(*  Version: 20110207                                                     *)
(*  Ported to Standard ML by Ivan Raikov (version 2014112)                *)
(*                                                                        *)
(*  The copyright to this code is held by Institut National de Recherche  *)
(*  en Informatique et en Automatique (INRIA). All rights reserved. This  *)
(*  file is distributed under the license CeCILL-C (see file LICENSE).    *)
(*                                                                        *)
(**************************************************************************)


(* This is an adaptation of Daan Leijen's [PPrint] library, which itself is
   based on the ideas developed by Philip Wadler in ``A Prettier Printer''.
   For more information, see:

     http://www.cs.uu.nl/~daan/pprint.html
     http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf *)

(* ------------------------------------------------------------------------- *)

exception AssertionFailed
exception NotFound
exception InvalidArgument

fun assert true = ()
  | assert false = raise AssertionFailed

(* Utility procedure to make a string of n repeating characters c *)
fun makeString c n = String.implode (List.tabulate (n, fn(i) => c))

(* Utility procedure to find the first occurrence of character c in string s,
   starting from index i*)
fun index_from s i c =
    let
        val slen = String.size s
        fun find (i) =
            if (i < slen)
            then (if String.sub(s,i) = c
                  then i else find (i+1))
            else raise NotFound
    in
        if ((slen = 0) orelse (slen < (i-1)))
        then raise InvalidArgument
        else find (i)
    end

(* A uniform interface for output channels. *)

signature OUTPUT = 
sig
  type channel
  val char: channel -> char -> unit
  val substring: channel -> string -> int (* offset *) -> int (* length *) -> unit
end

(* A signature for document renderers. *)

signature RENDERER =
sig
  
  (* Output channels. *)

  type channel
  type document

  (* [pretty rfrac width channel document] pretty-prints the document *)
  (*      [document] to the output channel [channel]. The parameter [width] is the *)
  (*      maximum number of characters per line. The parameter [rfrac] is the *)
  (*      ribbon width, a fraction relative to [width]. The ribbon width is the *)
  (*      maximum number of non-indentation characters per line. *)

  val pretty: real -> int -> channel -> document -> unit
                                                         
  (* [compact channel document] prints the document [document] to the output *)
  (*      channel [channel]. No indentation is used. All newline instructions are *)
  (*      respected, that is, no groups are flattened. *)
                                                         
  val compact: channel -> document -> unit

end

(* ------------------------------------------------------------------------- *)

(* (* A signature for representations of programming language values. *) *)

signature REPRESENTATION =
sig
    (* The type of value representation *)
    type representation

    type constructor
    type type_name
    type record_field
    type tag

    (* [variant type_name data_constructor_name tag arguments] *)
    (*         Given information about the variant and its arguments, *)
    (*         this function produces a new value representation. *)
    val variant : type_name -> constructor -> tag -> representation list -> representation

    (* [record type_name fields] *)
    (*         Given a type name and a list of record fields, this function *)
    (*         produces the value representation of a record. *)
    val record : type_name -> (record_field * representation) list -> representation

    (* [tuple arguments] *)
    (*         Given a list of value representation this function produces *)
    (*         a new value representation. *)
    val tuple : representation list -> representation
                                           
    (* Value representation for primitive types. *)
                                           
    val string : string -> representation
    val int : int -> representation
    val real : real -> representation
    val char : char -> representation
    val bool : bool -> representation
    val option : ('a -> representation) -> 'a option -> representation
    val list : ('a -> representation) -> 'a list -> representation
    val array : ('a -> representation) -> 'a array -> representation
    val ref_ : ('a -> representation) -> 'a ref -> representation

    (* Value representation for any other value. *)
    val unknown : type_name -> 'a -> representation
end

(* ------------------------------------------------------------------------- *) 

(* An implementation of the OUTPUT interface, based on TextIO.  *) 

structure ChannelOutput : OUTPUT =
struct
  type channel = TextIO.outstream
  fun char out (c: char) = TextIO.output1 (out, c)
  fun substring c str offset length =
      TextIO.outputSubstr (c, Substring.substring (str,offset,length))
end

(* ------------------------------------------------------------------------- *)

(* Here is the algebraic data type of documents. It is analogous to Daan *)
(* Leijen's version, but the binary constructor [Union] is replaced with *)
(* the unary constructor [Group], and the constant [Line] is replaced with *)
(* more general constructions, namely [IfFlat], which provides alternative *)
(* forms depending on the current flattening mode, and [HardLine], which *)
(* represents a newline character, and is invalid in flattening mode. *)

structure PPrintDocument =
struct

datatype document =

         (* [Empty] is the empty document. *)
         
         Empty
             
         (* [Char c] is a document that consists of the single
            character [c]. We enforce the invariant that [c] is not a
            newline character. *)
             
         | Char of char
                       
         (* [String (s, ofs, len)] is a document that consists of the
             portion of the string [s] delimited by the offset [ofs]
             and the length [len]. We assume, but do not check, that
             this portion does not contain a newline character. *)
                       
         | String of string * int * int
                                        
         (* [Blank n] is a document that consists of [n] blank characters. *)
                                        
         | Blank of int
                        
         (* When in flattening mode, [IfFlat (d1, d2)] turns into the document *)
         (*        [d1]. When not in flattening mode, it turns into the document [d2]. *)
                        
         | IfFlat of document * document
                                    
         (* When in flattening mode, [HardLine] is illegal. When not in flattening *)
         (*        mode, it represents a newline character, followed with an appropriate *)
         (*        number of indentation. A safe way of using [HardLine] is to only use it *)
         (*        directly within the right branch of an [IfFlat] construct. *)
                                    
         | HardLine
               
         (* [Cat doc1 doc2] is the concatenation of the documents [doc1] and *)
         (*        [doc2]. *)
               
         | Cat of document * document
                                 
         (* [Nest (j, doc)] is the document [doc], in which the indentation level *)
         (*        has been increased by [j], that is, in which [j] blanks have been *)
         (*        inserted after every newline character. *)
                                 
         | Nest of int * document
                             
         (* [Group doc] represents an alternative: it is either a flattened form of *)
         (*        [doc], in which occurrences of [Group] disappear and occurrences of *)
         (*        [IfFlat] resolve to their left branch, or [doc] itself. *)
                             
         | Group of document
                        
         (* [Column f] is the document obtained by applying [f] to the current *)
         (*        column number. *)
                        
         | Column of (int -> document)
                         
         (* [Nesting f] is the document obtained by applying [f] to the current *)
         (*        indentation level, that is, the number of blanks that were printed *)
         (*        at the beginning of the current line. *)
                         
         | Nesting of (int -> document)


(* ------------------------------------------------------------------------- *)

(* Constructors. *)
                          
val empty = Empty

fun ^^ (x, y) =
    (case (x, y) of
         (Empty, x) => x
       | (x, Empty) => x
       | (_, _) =>  Cat (x, y))

infix ^^

fun ifflat doc1 doc2 = IfFlat (doc1, doc2)

val hardline = HardLine

fun char c =
    (assert (not (c = #"\n"));
     Char c)

fun substring s ofs len =
  if len = 0
  then
      Empty
  else
      String (s, ofs, len)

fun text s =
    substring s 0 (String.size s)

fun blank n =
    if n = 0
    then
        Empty
    else
        Blank n

fun nest i x =
    (assert (i >= 0);
     Nest (i, x))

fun column f = Column f

fun nesting f = Nesting f

fun group x = Group x

(* ------------------------------------------------------------------------- *)

(* Low-level combinators for alignment and indentation. *)

fun align d = column (fn k => nesting (fn i => nest (k - i) d))

fun hang i d = align (nest i d)

fun indent i d = hang i (blank i ^^ d)

(* ------------------------------------------------------------------------- *)

(* High-level combinators. *)

val lparen          = char #"("
val rparen          = char #")"
val langle          = char #"<"
val rangle          = char #">"
val lbrace          = char #"{"
val rbrace          = char #"}"
val lbracket        = char #"["
val rbracket        = char #"]"
val squote          = char #"'"
val dquote          = char #"\""
val bquote          = char #"`"
val semi            = char #";"
val colon           = char #":"
val comma           = char #","
val space           = char #" "
val dot             = char #"."
val sharp           = char #"#"
val backslash       = char #"\\"
val equals          = char #"="
val qmark           = char #"?"
val tilde           = char #"~"
val at              = char #"@"
val percent         = char #"%"
val dollar          = char #"$"
val caret           = char #"^"
val ampersand       = char #"&"
val star            = char #"*"
val plus            = char #"+"
val minus           = char #"-"
val underscore      = char #"_"
val bang            = char #"!"
val bar             = char #"|"

fun break i      = ifflat (text (makeString #" " i)) hardline
val break0       = ifflat empty hardline
val break1       = ifflat space hardline


fun string s =
  let
      val n = String.size s
      fun chop i =
          let
              val j = index_from s i #"\n"
          in
              substring s i (j - i) ^^ break1 ^^ chop (j + 1)
          end
          handle NotFound => substring s i (n - i)
                                                                 
  in
      chop 0
  end

val group_break1 = group break1

fun words s =
  let
      val n = String.size s
      fun isblank c =
          case c of
              #" " => true
            | #"\t" => true
            | #"\n" => true
            | #"\r" => true
            | _ => false
              
      fun blank accu i = (* we have skipped over at least one blank character *)
          (if i = n
           then accu ^^ group_break1
           else
               (if isblank (String.sub (s,i))
	        then blank accu (i + 1)
                else word break1 accu i (i + 1)))

      and word prefix accu i j = (* we have skipped over at least one non-blank character *)
          (if j = n
           then accu ^^ group (prefix ^^ substring s i (j - i))
           else
               (if isblank (String.sub (s,j))
	        then blank (accu ^^ group (prefix ^^ substring s i (j - i))) (j + 1)
	        else word prefix accu i (j + 1)))
  in
      if n = 0
      then empty
      else
          (if isblank (String.sub (s,0))
	   then blank empty 1
	   else word empty empty 0 1)
  end

fun enclose l r x   = l ^^ x ^^ r

val squotes         = enclose squote squote
val dquotes         = enclose dquote dquote
val bquotes         = enclose bquote bquote
val braces          = enclose lbrace rbrace
val parens          = enclose lparen rparen
val angles          = enclose langle rangle
val brackets        = enclose lbracket rbracket


fun fold f (docs: document list) = List.foldr f empty docs


fun fold1 f docs =
   case docs of
       [] => empty
     | [ doc ] => doc
     | (doc :: docs) => f doc (fold1 f docs)


fun fold1map f g docs =
    case docs of
      [] => empty
     | [ doc ] => g doc
     | (doc :: docs) =>
       let
           val doc = g doc
       in (* force left-to-right evaluation *)
           f doc (fold1map f g docs)
       end

fun sepmap sep g docs = fold1map (fn (x) => (fn (y) => x ^^ sep ^^ y)) g docs

fun group1 d = group (nest 1 d)
fun group2 d = group (nest 2 d)

(* *)
(* structure Operators =  *)
(* struct *)
(*   val ( !^ ) = text *)
(*   let ( ^^ ) = ( ^^ ) *)
(*   let ( ^/^ ) x y = x ^^ break1 ^^ y *)
(*   let ( ^//^ ) x y = group (x ^^ nest 2 (break1 ^^ y)) *)
(*   let ( ^@^ ) x y = group (x ^^ break1 ^^ y) *)
(*   let ( ^@@^ ) x y = group2 (x ^^ break1 ^^ y) *)
(* end *)
(* *)

fun surround n sep open_doc contents close_doc =
    group (open_doc ^^ nest n (sep ^^ contents) ^^ sep ^^ close_doc)

fun surround1 open_txt contents close_txt =
    surround 1 break0 (text open_txt) contents (text close_txt)

fun surround2 open_txt contents close_txt =
    surround 2 break1 (text open_txt) contents (text close_txt)

fun soft_surround n sep open_doc contents close_doc =
    group (open_doc ^^ nest n (group sep ^^ contents) ^^
                    group (sep ^^ close_doc))

fun seq indent break empty_seq open_seq sep_seq close_seq xs =
    case xs of
        [] => empty_seq
      | _ =>
        surround indent break open_seq
                 (fold1 (fn (x) => (fn (xs) => x ^^ sep_seq ^^ xs)) xs)
                 close_seq

fun seq1 open_txt sep_txt close_txt =
    seq 1 break0 (text (open_txt ^ close_txt)) (text open_txt) ((text sep_txt) ^^ break1) (text close_txt)

fun seq2 open_txt sep_txt close_txt =
    seq 2 break1 (text (open_txt ^ close_txt)) (text open_txt) ((text sep_txt) ^^ break1) (text close_txt)

end                          

(* ------------------------------------------------------------------------- *)

(* The renderer is parameterized over an implementation of output channels. *)

functor PPrintRenderer (Output : OUTPUT) =
        struct

        open PPrintDocument

        (* The pretty rendering algorithm: preliminary declarations. *)

        (* The renderer is supposed to behave exactly like Daan Leijen's, although its *)
        (*    implementation is quite radically different. Instead of relying on *)
        (*    Haskell's lazy evaluation mechanism, we implement an abstract machine with *)
        (*    mutable current state, forking, backtracking (via an explicit stack of *)
        (*    choice points), and cut (disposal of earlier choice points). *)
                 
        (* The renderer's input consists of an ordered sequence of documents. Each *)
        (*    document carries an extra indentation level, akin to an implicit [Nest] *)
        (*    constructor, and a ``flattening'' flag, which, if set, means that this *)
        (*    document should be printed in flattening mode. *)

        (* An alternative coding style would be to avoid decorating each input *)
        (*    document with an indentation level and a flattening mode, and allow *)
        (*    the input sequence to contain instructions that set the current *)
        (*    nesting level or reset the flattening mode. That would perhaps be *)
        (*    slightly more readable, and slightly less efficient. *)

        datatype input = INil | ICons of int * bool * document * input

        (* When possible (that is, when the stack is empty), the renderer writes *)
        (*    directly to the output channel. Otherwise, output is buffered until either *)
        (*    a failure point is reached (then, the buffered output is discarded) or a *)
        (*    cut is reached (then, all buffered output is committed to the output *)
        (*    channel). At all times, the length of the buffered output is at most one *)
        (*    line. *)

        (* The buffered output consists of a list of characters and strings. It is *)
        (*    stored in reverse order (the head of the list should be printed last). *)

        datatype output = 
                 OEmpty
                 | OChar of char * output
                 | OString of string * int * int * output
                 | OBlank of int * output

        (* The renderer maintains the following state record. For efficiency, the *)
        (*    record is mutable; it is copied when the renderer forks, that is, at *)
        (*    choice points. *)

        type 'channel state = {

            (* The line width and ribbon width. *)

            width: int,
            ribbon: int,

            (* The output channel. *)

            channel: 'channel,

            (* The current indentation level. This is the number of blanks that *)
            (*        were printed at the beginning of the current line. *)

            indentation: int ref,

            (* The current column. *)

            column: int ref,

            (* The renderer's input. For efficiency, the input is assumed to never be *)
            (*        empty, and the leading [ICons] constructor is inlined within the state *)
            (*        record. In other words, the fields [nest1], [flatten1], and [input1] *)
            (*        concern the first input document, and the field [input] contains the *)
            (*        rest of the input sequence. *)

            indent1: int ref,
            flatten1: bool ref,
            input1: document ref,
            input: input ref,

            (* The renderer's buffer output. *)

            output: output ref

        }

        (* The renderer maintains a stack of resumptions, that is, states in which *)
        (*    execution should be resumed if the current thread of execution fails by *)
        (*    lack of space on the current line. *)

        (* It is not difficult to prove that the stack is empty if and only if *)
        (*    flattening mode is off. Furthermore, when flattening mode is on, *)
        (*    all groups are ignored, so no new choice points are pushed onto the *)
        (*    stack. As a result, the stack has height one at most at all times, *)
        (*    so that the stack height is zero when flattening mode is off and *)
        (*    one when flattening mode is on. *)

        type 'channel stack = 'channel state list

        (* ------------------------------------------------------------------------- *)


        type channel =
             Output.channel

        (* Printing blank space (indentation characters). *)

        val blank_length = 80

        val blank_buffer = makeString #" " blank_length

        fun blanks channel n =
            (if n <= 0
             then ()
             else
                 if n <= blank_length
                 then
                     Output.substring channel blank_buffer 0 n
                 else
                     (Output.substring channel blank_buffer 0 blank_length;
                      blanks channel (n - blank_length)))

        (* Committing buffered output to the output channel. The list is printed in *)
        (*      reverse order. The code is not tail recursive, but there is no risk of *)
        (*      stack overflow, since the length of the buffered output cannot exceed one *)
        (*      line. *)

        fun commit channel output =
            (case output of
                 OEmpty => ()
               | OChar (c, output) =>
	         (commit channel output;
                  Output.char channel c)
               | OString (s, ofs, len, output) =>
	         (commit channel output;
                  Output.substring channel s ofs len)
               | OBlank (n, output) =>
	         (commit channel output;
                  blanks channel n))

        (* The renderer's abstract machine. *)

        (* The procedures [run], [shift], [emit_char], [emit_string], and *)
        (*      [emit_blanks] are mutually recursive, and are tail recursive. They *)
        (*      maintain a stack and a current state. The states in the stack, and the *)
        (*      current state, are pairwise distinct, so that the current state can be *)
        (*      mutated without affecting the contents of the stack. *)

        (* An invariant is: the buffered output is nonempty only when the stack is *)
        (*      nonempty. The contrapositive is: if the stack is empty, then the buffered *)
        (*      output is empty. Indeed, the fact that the stack is empty means that no *)
        (*      choices were made, so we are not in a speculative mode of execution: as a *)
        (*      result, all output can be sent directly to the output channel. On the *)
        (*      contrary, when the stack is nonempty, there is a possibility that we *)
        (*      might backtrack in the future, so all output should be held in a *)
        (*      buffer. *)

        (* [run] is allowed to call itself recursively only when no material is *)
        (*      printed.  In that case, the check for failure is skipped -- indeed, *)
        (*      this test is performed only within [shift]. *)

        fun run (stack : channel stack) (state : channel state) : unit =

            (* Examine the first piece of input, as well as (in some cases) the *)
            (*        current flattening mode. *)

            (case (!(#input1(state)), !(#flatten1(state))) of

                 (* The first piece of input is an empty document. Discard it *)
                 (*        and continue. *)

                 (Empty, _) =>
                 shift stack state
                       
               (* The first piece of input is a character. Emit it and continue. *)
                       
               | (Char c, _) =>
                 emit_char stack state c
                           
               (* The first piece of input is a string. Emit it and continue. *)
                           
               | (String (s, ofs, len), _) =>
                 emit_string stack state s ofs len
                             
               | (Blank n, _) =>
                 emit_blanks stack state n
                             
               (* The first piece of input is a hard newline instruction. Such an *)
               (*        instruction is valid only when flattening mode is off. *)
                             
               (* We emit a newline character, followed by the prescribed amount of *)
               (*        indentation. We update the current state to record how many *)
               (*        indentation characters were printed and to to reflect the new *)
               (*        column number. Then, we discard the current piece of input and *)
               (*        continue. *)
                             
               | (HardLine, flattening) =>
                 (assert (not flattening); (* flattening mode must be off. *)
                  assert (List.null stack);     (* since flattening mode is off, the stack must be empty. *)
                  Output.char (#channel(state)) #"\n";
                  let
                      val i = !(#indent1(state))
                  in
	              (blanks (#channel(state)) i;
	               #column(state) := i;
	               #indentation(state) := i;
	               shift stack state)
                  end)
                     
               (* The first piece of input is an [IfFlat] conditional instruction. *)
                     
               | (IfFlat (doc, _), true) =>
                 (#input1(state) := doc;
                  run stack state)

               | (IfFlat (_, doc), false) =>
                 (#input1(state) := doc;
                  run stack state)
                     
               (* The first piece of input is a concatenation operator. We take it *)
               (*        apart and queue both documents in the input sequence. *)
                     
               | (Cat (doc1, doc2), _) =>
	         (#input1(state) := doc1;
	          #input(state)  := ICons (!(#indent1(state)), !(#flatten1(state)),
                                           doc2, !(#input(state)));
	          run stack state)
                     
               (* The first piece of input is a [Nest] operator. We increase the amount *)
               (*        of indentation to be applied to the first input document. *)
                     
               | (Nest (j, doc), _) =>
                 (#indent1(state) := (!(#indent1(state))) + j;
                  #input1(state)  := doc;
                  run stack state)

               (* The first piece of input is a [Group] operator, and flattening mode *)
               (*        is currently off. This introduces a choice point: either we flatten *)
               (*        this whole group, or we don't. We try the former possibility first: *)
               (*        this is done by enabling flattening mode. Should this avenue fail, *)
               (*        we push the current state, in which flattening mode is disabled, *)
               (*        onto the stack. *)

               (* Note that the current state is copied before continuing, so that *)
               (*        the state that is pushed on the stack is not affected by future *)
               (*        modifications. This is a fork. *)

               | (Group doc, false) =>
                 let
                     val {width,ribbon,channel,indentation,column,
                          indent1,flatten1,input1,input,output} = state
                     val state' = {width=width,ribbon=ribbon,channel=channel,indentation=ref(!indentation),
                                   column=ref(!column),indent1=ref(!indent1),flatten1=ref true,
                                   input1=ref(!input1),input=ref(!input),output=ref(!output)}
                 in
                     (#input1(state) := doc;
                      run (state :: stack) state')
                 end

               (* The first piece of input is a [Group] operator, and flattening mode *)
               (*        is currently on. The operator is ignored. *)

               | (Group doc, true) =>
                 (#input1(state) := doc;
                  run stack state)

               (* The first piece of input is a [Column] operator. The current column *)
               (*        is fed into it, so as to produce a document, with which we continue. *)

               | (Column f, _) =>
                 (#input1(state) := f (!(#column(state)));
                  run stack state)

               (* The first piece of input is a [Column] operator. The current *)
               (*        indentation level is fed into it, so as to produce a document, with *)
               (*        which we continue. *)

               | (Nesting f, _) =>
                 (#input1(state) := f (!(#indentation(state)));
                  run stack state))

            (* [shift] discards the first document in the input sequence, so that the *)
            (*      second input document, if there is one, becomes first. The renderer stops *)
            (*      if there is none. *)

            and shift stack state =

                (assert ((!(#output(state)) = OEmpty) orelse (not (List.null stack)));
                 assert (!(#flatten1(state)) = (not (List.null stack)));
                 
                 (* If the stack is nonempty and we have exceeded either the width or the *)
                 (*        ribbon width parameters, then fail. Backtracking is implemented by *)
                 (*        discarding the current state, popping a state off the stack, and making *)
                 (*        it the current state. *)
                 
                 if ((not (null stack))
                     andalso ((!(#column(state)) > #width(state)) orelse
                              ((!(#column(state)) - (!(#indentation(state)))) > (#ribbon(state)))))
                 then
	             run (tl stack) (hd stack)
                 else
	             (case (!(#input(state))) of
                          
	                  INil =>
                          
	                  (* End of input. Commit any buffered output and stop. *)
                          
	                  commit (#channel(state)) (!(#output(state)))
                                 
	                | ICons (indent, flatten, head, tail) =>
                          
	                  (* There is an input document. Move it one slot ahead and *)
                          (* 	       check if we are leaving flattening mode. *)
                          
	                  (#indent1(state) := indent;
	                   #input1(state)  := head;
	                   #input(state)   := tail;
                           
	                   if ((!(#flatten1(state))) andalso (not flatten))
                           then
                               
	                       (* Leaving flattening mode means success: we have flattened *)
                               (* 		 a certain group, and fitted it all on a line, without *)
                               (* 		 reaching a failure point. We would now like to commit our *)
                               (* 		 decision to flatten this group. This is a Prolog cut. We *)
                               (* 		 discard the stack of choice points, replacing it with an *)
                               (* 		 empty stack, and commit all buffered output. *)
                               
	                       (#flatten1(state) := flatten; (* false *)
	                        commit (#channel(state)) (!(#output(state)));
	                        #output(state) := OEmpty;
	                        run [] state)
                                   
	                   else
	                       run stack state))
                )
                (* [emit_char] prints a character (either to the output channel or to the *)
                (*      output buffer), increments the current column, discards the first piece *)
                (*      of input, and continues. *)
                    
            and emit_char stack state c =

                (case stack of
                     [] => Output.char (#channel(state)) c
                   | _ => #output(state) := OChar (c, (!(#output(state))));
                 #column(state) := !(#column(state)) + 1;
                 shift stack state)
                    
            (* [emit_string] prints a string (either to the output channel or to the *)
            (*      output buffer), updates the current column, discards the first piece of *)
            (*      input, and continues. *)
                    
            and emit_string stack state s ofs len =
                (case stack of
                     [] => Output.substring (#channel(state)) s ofs len
                   | _ => #output(state) := OString (s, ofs, len, (!(#output(state))));
                 #column(state) := !(#column(state)) + len;
                 shift stack state)
                    
            (* [emit_blanks] prints a blank string (either to the output channel or to *)
            (*      the output buffer), updates the current column, discards the first piece *)
            (*      of input, and continues. *)
                    
            and emit_blanks stack state n =
                ((case stack of
                      [] => blanks (#channel(state)) n
                    | _ => #output(state) := OBlank (n, (!(#output(state)))));
                 #column(state) := !(#column(state)) + n;
                 shift stack state)
                    
        (* This is the renderer's main entry point. *)

        fun pretty rfrac width channel (document: document) =
            let
                val state: channel state =
                    {
                      width = width,
                      ribbon = Int.max (0, (Int.min (width, (Real.trunc (Real.* (Real.fromInt(width), rfrac)))))),
                      channel = channel,
                      indentation = ref 0,
                      column = ref 0,
                      indent1 = ref 0,
                      flatten1 = ref false,
                      input1 = ref document,
                      input = ref INil,
                      output = ref OEmpty
                    }
            in
                run [] state
            end

        (* ------------------------------------------------------------------------- *)

        (* The compact rendering algorithm. *)

        fun compact channel document =

            let
                val column = ref 0

                fun scan x =
                    case x of
                        Empty => ()
                      | Char c =>
	                (Output.char channel c;
	                 column := !column + 1)
                      | String (s, ofs, len) =>
	                (Output.substring channel s ofs len;
	                 column := !column + len)
                      | Blank n =>
	                (blanks channel n;
	                 column := !column + n)
                      | HardLine =>
	                (Output.char channel #"\n";
	                 column := 0)
                      | Cat (doc1, doc2) =>
	                (scan doc1; scan doc2)
                      | IfFlat (doc, _) =>
                        scan doc
                      | Nest (_, doc) =>
                        scan doc
                      | Group doc =>
	                scan doc
                      | Column f =>
	                scan (f (!column))
                      | Nesting f =>
	                scan (f 0)
            in
                
                scan document

            end

        end

(* ------------------------------------------------------------------------- *)

(* Instantiating the renderers for the different kinds of output channels. *) 

structure ChannelRenderer = PPrintRenderer(ChannelOutput)

