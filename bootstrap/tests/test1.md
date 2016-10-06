# Sobre este archivo

Este archivo contiene únicamente código en *markdown* acompañado de algunas secuencias se caracteres "escapadas" de la sintaxis de `sweb`.

Las secuencias de caracteres que necesitan ser escapadas son:

- `@<<`
- `@>>`
- `@@`
- `@%def`

El caracter de escape que se utiliza en `sweb` es `@@` por lo tanto, para escribir @<< en un programa de `sweb` se debe incluír el prefijo `@@`, es decir, escribir `@@@<<`.
