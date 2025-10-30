## TODO
DONE - readme for read_tymelive & read_parvo
- testthat for helper functions
- testthat for read_tymewear.post
DONE - testthat for read_parvo
DONE - -- details$name works with first & last / first only / last only

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
or create new working directory for R package
- delete namespace, build from scratch

2.
usethis::use_git()
usethis::usegithub()
- happygitwithr.com by Jenny Bryan
- committ early & often

3.
usethis::use_mit_license()

4.
usethis::use_r("fun_name")
- create minimal R function

5.
devtools::load_all() ## ctrl+shift+l
- load all imported & exported functions to test

6. insert roxygen2 comment (ctrl+alt+shift+R)
@title @description @param @details @return @seealso @examples @importFrom @export
@importFrom (specific functions) @import (whole package)

7.
usethis::use_air() ## for external formatting


8.
usethis::use_package_doc()
usethis::use_package("scales", type = "Suggests")
- add external package as dependency
usethis::use_import_from("cli", "cli_warn")
usethis::use_tidy_description() ## to format DESCRIPTION

9.
devtools::document() ## ctrl+shift+d

10.
devtools::install() ## ctrl+shift+b

11.
usethis::use_test()
- for open file
- or
usethis::use_test("theme_epl")

12.
devtools::load_all() ## ctrl+shift+l then  for all function tests
devtools::test()
devtools::check() ## will run tests
- check early & often

13.
usethis::use_readme_rmd()
devtools::build_readme()
- Package description & basic use overview

14.- dont quite know how to implement this yet
usethis::use_coverage() ## reports test coverage
# usethis::use_github_action()
usethis::use_github_action("check-standard") ## Run `R CMD check` on Linux, macOS, and Windows
usethis::use_github_action("test-coverage") ## Compute test coverage and report to https://about.codecov.io
-  report the R CMD check status of your development package.

15.
usethis::use_vignette("reading-data.qmd")
devtools::build_vignettes() ## Build vignettes if any
devtools::clean_vignettes()

16.
usethis::use_pkgdown_github_pages() ## same as usethis::use_pkgdown from GitHub Actions
pkgdown::build_reference(run_dont_run = TRUE)
# usethis::use_github_action("pkgdown") <what is this?>
- NEED TO REMOVE `docs` FROM GITIGNORE
devtools::build_site() ## render local site
pkgdown::clean_site() ## to remove pkgdown site

17.
usethis::use_news_md() ## to initiate the NEWS.md file
usethis::use_lifecycle()
usethis::use_lifecycle_badge("experimental")
usethis::use_version("patch")
usethis::use_dev_version()
usethis::use_release_issue()
- <add description>

18.
remotes::install_github("jemarnold/epl") ## install from github


## Claude.ai Pre-release checklist ===============================
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

