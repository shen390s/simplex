Simple LaTeX.

Current version is v0.4.0.

See Simplex-v0.4.0.pdf for more information
and more detailed installation instructions.

What's new in v0.4.0
=====================

  * New diagram and plotting verbatim blocks in addition to
    the existing Graphviz blocks (`.digraph`, `.graph`,
    `.neato`, `.dot`):

        .gnuplot    plot with gnuplot
        .mermaid    diagrams with the mermaid CLI (mmdc)
        .ditaa      ASCII-art diagrams with ditaa
        .plantuml   UML diagrams with PlantUML

    Graphviz diagrams are now rendered as JPEG.

  * HTML output. Use `--type=html`; the document is built with
    `lwarp` and `latexmk`. New directives control the HTML:

        @filedepth            how deep the HTML is split into files
        @sidetocdepth         depth of the side table of contents
        @nofilesectionnames   do not use section names in file names
        @css                  custom stylesheet for the HTML output

  * CJK / Chinese support. Add `@cjk` to your document and it is
    built with XeLaTeX using the `ctex` package. Fonts are now
    selected through an `iftex`/`fontspec` mechanism, so system
    fonts can be chosen from `@preamble` when building with
    XeLaTeX or LuaLaTeX.

  * Word-style section aliases as readable alternatives to the
    terse symbol markers:

        .part          !!!
        .chapter       !!
        .section       =
        .subsection    ==
        .subsubsection ===

  * New command line options `--java`, `--mmdc`, `--ditaa` and
    `--plantuml` to point at the external tools.

  * Editor support: a tree-sitter grammar (in `tree-sitter-simplex/`)
    and an Emacs major mode `simplex-ts-mode` (in `emacs/`) for
    editing `.simplex` files, plus a Nix flake for reproducible
    builds (`nix build`).

Installing
==========

In order to build simplex you need the Haskell Platform.
You can download it at http://www.haskell.org/platform/ 

You can use the Haskell Cabal to install the package,
for example like so:

    cabal install simplex

This is similar to `./configure; make; make install`.

simplex requires a latex distribution which provides the
`pdflatex` command. TeX Live is recommended, as it provides
all packages that simplex uses.

For certain features you will also need `graphviz` and
`ImageMagick`, but simplex will run without. The optional
diagram and plotting blocks additionally require `gnuplot`
(for `.gnuplot`), the mermaid CLI `mmdc` (for `.mermaid`),
and a Java runtime plus `ditaa.jar` / `plantuml.jar`
(for `.ditaa` and `.plantuml`). HTML output requires the
`lwarp` package and `latexmk`.

Building with Nix
-----------------

A Nix flake is provided. If you have Nix with flakes enabled
you can build simplex, with all of its runtime dependencies
wrapped in (TeX Live, Graphviz, gnuplot, ImageMagick, ...),
with:

    nix build

The resulting executable is placed at `result/bin/simplex`.

On Ubuntu
---------

On Ubuntu you will have to install the following packages:

    haskell-platform
    texlive
    texlive-latex-extra
    texlive-math-extra
    latex-xcolor

You should also install:

    graphviz

You might need to run `cabal update` once. For a global
installation use `sudo cabal install --global`. This will
install the `simplex` executable in a folder on your `$PATH`.

Alternatively you might just want to add it to your `$PATH`
in your `.bashrc`:

`export PATH=$PATH:~/Library/Haskell/bin`.

On Mac OS X
-----------

For Mac OS X there is Mac TeX (http://www.tug.org/mactex/),
which should include all relevant packages. Graphviz can be
obtained from http://www.graphviz.org/Download_macos.php .

If you are using Homebrew (http://mxcl.github.com/homebrew/)
you might want to install ImageMagick and Graphviz like so:

    brew install graphviz
    brew install imagemagick

You might want to add it to your `$PATH` in your `.bash_login`:

`export PATH=$PATH:~/Library/Haskell/bin`.

Using Simplex
=============

simplex will automatically process all files in the
current working directory, but you may also specify
which files to process. Here is what simplex will tell
you when you ask it for help (`--help` or `-h`):

    simplex [options] [files...]

      -h        --help              Print this help text.
      -V        --version           Print version information.
      -v        --verbose           Verbose output.
      -d        --dry-run           Dry run (do not create any files).
      -n        --no-clean          Do not clean up after building.
      -p        --print             Print processed tex to stdout.
      -c        --crop              Crops the document so that no margins are left.
      -f        --force             Forces the creation of output files.
      -t , -T   --type=             Specify type of output (pdf, png, tex, html)
      -x        --pdflatex=         Path to `pdflatex' executable
      -k        --pdfcrop=          Path to `pdfcrop'
      -z        --graphviz=         Path to `dot' (graphviz)
      -g        --gnuplot=          Path to `gnuplot'
      -m        --convert=          Path to `convert' (ImageMagick)
                --java=             Path to `java'
                --mmdc=             Path to `mmdc' (mermaid)
                --ditaa=            Path to ditaa.jar
                --plantuml=         Path to plantuml.jar
      -w[]      --watch[=]          Watch files or folder (optionally amount of time in ms)
      -3        --three-times       Execute `pdflatex' three times instead of the default two times.
      -s[]      --symbols[=]        Show a list of symbols known to simplex.
                --density=, --dpi=  For output type `png' only, specifies dpi.
                --quality=          For output type `png' only, specifies quality.


