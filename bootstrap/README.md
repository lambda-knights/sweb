# Bootstrap

La herramienta `sweb` está implementada usando programación literaria con sintaxis de Markdown, TeX, LaTeX y ConTeXt para la prosa, la sintaxis de `cweb` para la delimitación de fragmentos y el lenguaje Scheme en los fragmentos de código.

Para poder extraer los archivos fuente es necesario *enmarañar* (`tangle`) los archivos fuente, sin embargo esto no es posible ya que este procedimiento es implementado en `sweb`.

Para poder extraer el código ejecutable de `sweb` se necesita código no literario que implemente la funcionalidad más *raspa* del procedimiento `tangle`. En este directorio se encuentran los códices que nos permiten hacer eso con un toque de magia.

Después de haber escrito la versión literaria y completa de `sweb` este código podrá ser desechado.

La estructura de este directorio es:

- `/bootstrap/`
  - `README.md` (este archivo)
  - `tangle.scm` (punto de entrada)
  - `/lib/`
    - `input.scm` (puertos de entrada como flujos)
    - `parcomb.scm` (combinadores de parsers)
    - `utils.scm` (código de utilería)
  - `/doc/`
    - `README.md` (documento del código basado en comentarios de bootstrap)
    - `README.md.pdf` (documento en PDF del archivo anterior para imprimir)
    - `gendoc.scm` (codigo para generar los anteriores archivos de forma automágica)
    - `preamble.scm` (tres líneas para hacer feliz a pandoc)
