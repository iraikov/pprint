
val expr =
    let
        open RepresentationC
    in
        proc "test" "int" 
             [("int", "x"),("int", "x")]  
             (binop "+" (id "x") (id "y"))
    end

val _ =
    (
      ChannelRenderer.compact TextIO.stdOut expr;
      ChannelRenderer.pretty 0.5 72 TextIO.stdOut expr
    )

