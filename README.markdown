# citeproc

citeproc is a standalone program that uses Andrea Rossato's [citeproc-hs]
to generate citations and a bibliography, given a database of bibliographic
references and a [CSL] stylesheet.  The idea is to make it possible for
non-Haskell programs to use this excellent tool.

This version is a fork of John MacFarlane's [citeproc] program.  The
major changes are:

- The input JSON format is compatible with the format used by [citeproc-js]
- This version renders output directly in a target format (currently, just
  ASCII or HTML) rather than as JSON

[citeproc-hs]: http://gorgias.mine.nu/repos/citeproc-hs/
[citeproc-js]: http://gsl-nagoya-u.net/http/pub/citeproc-doc.html
[CSL]: http://citationstyles.org/
[citeproc]: https://github.com/jgm/citeproc 

## Usage

    citeproc format stylefile.csl bibliofile.bib

- The bibliography file can be in any of the formats that bibutils
  supports.
- The format can be `ascii` or `html`.
- The program reads JSON from stdin and writes formatted data to stdout.

## Input

The input is a JSON array of citations.
A citation is a JSON array of cites.
A cite is a JSON object describing one source.  Fields can include:

- `id` (string)
- `prefix` (string or array)
- `suffix` (string or array)
- `label` (string)
- `locator` (string)
- `suppress-author` (boolean)
- `author-in-text` (boolean)

`id` must be included; the rest are optional.

## Installing citeproc

Change to the source directory and:

    cabal install

