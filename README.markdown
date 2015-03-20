# org-citeproc

org-citeproc is a standalone program that uses [pandoc-citeproc]
to generate citations and a bibliography, given a database of bibliographic
references and a [CSL] stylesheet.  The idea is to make it possible for
Emacs' [Org mode] exporters to use this excellent tool.  

org-citeproc is a fork of John MacFarlane's [citeproc] program.  The
major changes are:

- The input JSON format is compatible with the format used by [citeproc-js]
- This version renders output directly in a target format (currently
  ASCII, HTML, or ODT) rather than as JSON

[pandoc-citeproc]: https://github.com/jgm/pandoc-citeproc
[citeproc-js]: http://gsl-nagoya-u.net/http/pub/citeproc-doc.html
[CSL]: http://citationstyles.org/
[Org mode]: http://orgmode.org
[citeproc]: https://github.com/jgm/citeproc 

## Usage

    org-citeproc format stylefile.csl bibliofile.bib

- The bibliography file can be in any of the formats that pandoc-citeproc
  supports.
- The format can be `ascii`, `html`, or `odt`.
- The program reads JSON from stdin and writes formatted data to stdout.

## Input

The input is an array of JSON citation data objects.  A citation data
object contains one field, `citationItems`, whose value is a JSON
array of cites.  A cite is a JSON object describing one source.
Fields in a cite can include:

- `id` (string)
- `prefix` (string)
- `suffix` (string)
- `label` (string)
- `locator` (string)
- `suppress-author` (boolean)
- `author-in-text` (boolean)

`id` must be included; the rest are optional.

See the `*-to-json` functions in the [org-cite] library for Elisp code
that generates JSON in this format, given Org citation objects.

[org-cite]: https://github.com/wyleyr/org-mode/blob/wip-cite-org-citeproc/lisp/org-cite.el

## Installing org-citeproc

Change to the source directory and run:

    cabal build

If you want to use org-citeproc with Org mode, copy the resulting
`org-citeproc` binary from `dist/build/org-citeproc/org-citeproc` to
`$ORGROOT/bin/org-citeproc`, where `$ORGROOT` is the root of your Org
mode distribution.  My [wip-cite-org-citeproc branch] of Org mode
makes use of this binary.

[wip-cite-org-citeproc branch]: https://github.com/wyleyr/org-mode/

## Contributing
There is a list of TODO items in [TODO.org].  Contributions of code, bug
reports, and so on are welcome!

[TODO.org]: ./TODO.org

