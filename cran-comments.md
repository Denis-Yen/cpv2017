## Submission

This is a new submission.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for the reviewers

* The documentation is in Spanish (`Language: es`), since the package is
  aimed at users of the Peruvian census.
* The full census data (~940 MB) is not included in the package. Users
  download it once with `cpv_descargar()` from a GitHub release; they are the official
  'REDATAM' database
  (<https://prod.redatam.org/cdr/descargas/censos/poblacion/CP2017PER.zip>), freely
  published by INEI and ECLAC, converted with `cpv_convertir()`, which any
  user can run to reproduce them. A small real sample
  (two districts, 184 KB) is included in `inst/extdata` so that all examples
  and tests run offline.
* `cpv_descargar()` writes by default to `tools::R_user_dir("cpv2017", "data")`
  and only when called by the user. Its example is wrapped in `\dontrun{}`
  because it downloads ~940 MB. The example of `cpv_convertir()` is wrapped
  in `\dontrun{}` because it needs the original REDATAM database (1.7 GB),
  which is not distributed with the package.
* Tests do not use the internet: the download is tested against a local
  `file://` URL in a temporary directory.
