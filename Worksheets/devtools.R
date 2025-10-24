## Building R packages with devtools and usethis | RStudio ================
## https://www.youtube.com/watch?v=EpTkT6Rkgbs
MVP minimum viable package
- metadata (DESCRIPTION)
- source code function.R
- roxygen comments in function.R
- NAMESPACE for imported and exported functions
- tests
- other (files, data, tutorials, vignettes)

The Whole Game
library(devtools) ## calls library(usethis)


1.
usethis::create_package()
or
1. create new working directory for R package
- delete namespace, build from scratch

2.
usethis::use_r("fun_name")
- create minimal R function

3.
devtools::load_all()
- load all imported & exported functions to test

4. insert roxygen2 comment (ctrl+alt+shift+R)
- @title @description @param @details @return @export @examples
- @importFrom (specific functions) @import (whole package)

5.
usethis::use_package("rlang", type = "Imports")
- add external package as dependency
usethis::use_version()

6.
devtools::document() ## ctrl+shift+d

7.
devtools::install() ## ctrl+shift+b

8.
devtools::check()
- check early & often
usethis::use_air() ## for external formatting

9.
devtools::build_vignettes() ## devtools::clean_vignettes()
devtools::build_readme()
- Build vignettes if any

10.
usethis::use_git()
usethis::usegithub()
- happygitwithr.com by Jenny Bryan
- committ early & often

11.
usethis::use_mit_license()

12.
usethis::use_test("read_tymewar")
- for open file
devtools::load_all() ## ctrl+shift+l then  for all function tests
devtools::test()
devtools::check() ## will run tests

13.
usethis::use_github_action("pkgdown")
usethis::use_pkgdown_github_pages()
- NEED TO REMOVE `docs` FROM GITIGNORE
devtools::build_site() ## locally build
pkgdown::clean_site() to remove pkgdown site

14.
remotes::install_github("jemarnold/mnirs")
