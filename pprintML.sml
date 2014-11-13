
structure RepresentationML: REPRESENTATION =
struct

  open PPrintDocument
  infix ^^

  type representation = document

  type constructor = string
  type type_name = string
  type record_field = string
  type tag = int


  val tuple = seq1 "(" "," ")"

  fun variant _ cons _ args =
      if List.null args
      then (text cons) 
      else (text cons) ^^ tuple args

  fun record _ fields =
      seq2 "{" "," "}" (List.map (fn (k, v) => (text k) ^^ (text "=") ^^ v) fields)

  fun option f x =
      case x of
          SOME x => (text "Some") ^^ tuple [f x]
        | NONE => (text "None")

  fun list f xs = seq2 "[" "," "]" (List.map f xs)

  fun array f xs = seq2 "[|" "," "|]" (Vector.foldr (op ::) [] (Vector.map f (Array.vector xs)))

  fun ref f x = record "ref" [("contents", f (!x))]
  fun real f = string (Real.toString f)

  fun int i = string (Int.toString i)
  fun char c = string (Char.toString c)
  fun bool b = string (Bool.toString b)
  fun string s = string s
  fun unknown tyname _ = string ("<" ^ tyname ^ ">")

end
