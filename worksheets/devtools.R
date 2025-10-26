## TODO
- readme for read_tymelive & read_parvo
- testthat for helper functions
- testthat for read_tymewear
- testthat for read_parvo
- -- details$name works with first & last / first only / last only

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
usethis::use_package_doc()
usethis::use_package("ggplot2")
usethis::use_import_from("rlang", "check_installed")
usethis::use_tidy_description() ## to format DESCRIPTION
usethis::
- add external package as dependency
 ## @import rlang tibble dplyr tidyr lubridate
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
devtools::build_readme()
devtools::build_vignettes() ## devtools::clean_vignettes()
- Build vignettes if any

10.
usethis::use_git()
usethis::usegithub()
- happygitwithr.com by Jenny Bryan
- committ early & often

11.
usethis::use_mit_license()

12.
usethis::use_test("theme_epl")
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


## Claude.ai Pre-release checklist
Essential:
    
* Version number in DESCRIPTION (start with 0.1.0 for initial release)
* LICENSE file (MIT or GPL-3 are common for academic work)
* All functions documented with roxygen2 comments
* NAMESPACE generated (run devtools::document())
* Basic README.md explaining installation and core usage
* Run devtools::check() with zero errors, warnings, or notes
* Remove any hardcoded file paths or credentials
* Add a NEWS.md to track changes between versions

Recommended:
    
* Function examples in documentation that actually run
* At least basic unit tests (using testthat)
* .Rbuildignore to exclude development files
* Specify minimum R version in DESCRIPTION
* Check for conflicting function names with common packages
* Add a vignette if the package workflow isnt obvious

Development workflow
Use Git branches with GitHub/GitLab:
* Main branch (main) = stable, public releases only
* Development branch (dev) = working version for you

```{r}
# Initial setup (once)
usethis::use_git()
usethis::use_github()  # or GitLab equivalent

# In terminal:
git checkout -b dev
git push -u origin dev
```

Your workflow:
    
* Make all changes on dev branch
* Push dev to remote after each work session
* Both your systems pull from dev
* When ready for public release: merge dev → main, increment version, push

Installation for colleagues:
    
```{r}
# From main (stable)
remotes::install_github("username/packagename")

# You can test dev version with:
remotes::install_github("username/packagename", ref = "dev")
```

Version numbering:
    
* Development: 0.1.0.9000 (add .9000 suffix on dev branch)
* Public releases: 0.1.0, 0.1.1 (bug fixes), 0.2.0 (new features)

This keeps incremental commits off the public release whilst maintaining sync across your systems.

