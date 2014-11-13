
structure RepresentationC =
struct

  open PPrintDocument
  infix ^^

  type representation = document

  type type_name = string
  type record_field = string

  fun id x = string x

  fun binop oper x y  = enclose x y (id oper)

  fun unop oper x  = oper ^^ x

  fun proc name ty args body =
      (text ty) ^^ space ^^ (id name) ^^ 
                (seq2 "(" "," ")" (List.map (fn (ty, x) => (id x) ^^ (text ": ") ^^ (text ty)) args)) ^^ 
                break1 ^^ soft_surround 2 break1 (text "{") body (text "}") ^^ hardline

  fun record _ fields =
      seq2 "{" "," "}" (List.map (fn (k, v) => (text k) ^^ (text "=") ^^ v) fields)

  fun array f xs = seq2 "[|" "," "|]" (Vector.foldr (op ::) [] (Vector.map f (Array.vector xs)))

  fun real f = string (Real.toString f)
  fun int i = string (Int.toString i)
  fun char c = string (Char.toString c)
  fun bool b = string (Bool.toString b)
  fun string s = string s
  fun unknown tyname _ = string ("<" ^ tyname ^ ">")

end
