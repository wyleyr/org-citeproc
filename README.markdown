# citeproc

citeproc is a standalone program that uses [pandoc-citeproc]
to generate citations and a bibliography, given a database of bibliographic
references and a [CSL] stylesheet.  The idea is to make it possible for
Emacs' [Org mode] exporters to use this excellent tool.

This version is a fork of John MacFarlane's [citeproc] program.  The
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

    citeproc format stylefile.csl bibliofile.bib

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

See [json.el] for Elisp code that generates JSON in this format, given
Org citation objects.

[json.el]: ./json.el

## Installing citeproc

Change to the source directory and:

    cabal install

