# Sobre este archivo

En este archivo se muestra la preservación de la indentación de referencias de código al ser procesadas por `tangle`

<<deleteme>>=
(begin
  <<Primero>>
  <<Segundo>>
  (begin
    <<Segundo>> <<Primero>>))
@

<<Primero>>=
(+ 1 2 3)
<<Segundo>>=
(+ 1
   2
   3)
@
