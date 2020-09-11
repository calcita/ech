## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

## resubmission

* Calls of on.exit() that the settings are reset when the function is exited.
* We use .archiveExtractBinary of reproducible package, but its an internal function, and we have not chance to import in our DESCRIPTION, then we write to its author, Tati Micheletti to tell her to include her as "ctb" (Contributor). Her answer:

"Absolutely! I'm glad the function is useful. You guys can use it the way you suggested and I am happy to be included as ctb. :) 
I'll chat with my colleagues about the possibility of making it an exportable function instead of an internal one in the next release of reproducible."
