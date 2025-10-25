test_that("example_epl() returns all files when file = NULL", {
    files <- example_epl()
    expect_type(files, "character")
    expect_true(length(files) > 0)
})

test_that("example_epl() returns valid path for exact match", {
    # Assumes at least one file exists in extdata
    all_files <- example_epl()
    skip_if(length(all_files) == 0, "No example files available")

    path <- example_epl(all_files[1])
    expect_type(path, "character")
    expect_true(file.exists(path))
    expect_match(path, all_files[1], fixed = TRUE)
})

test_that("example_epl() returns valid path for partial match", {
    skip_if_not(any(grepl("tymewear_live", example_epl(), fixed = TRUE)),
                "tymewear_live.csv not available")

    path <- example_epl("tymewear_live")
    expect_true(file.exists(path))
    expect_match(path, "tymewear_live", fixed = TRUE)
})

test_that("example_epl() errors on multiple partial matches", {
    skip_if_not(sum(grepl("tymewear", example_epl(), fixed = TRUE)) > 1,
                "Multiple tymewear files not available")

    expect_error(example_epl("tymewear"), "Multiple files match")
})

test_that("example_epl() errors on non-existent file", {
    expect_error(example_epl("nonexistent_file_xyz"), "'arg' should be one of")
})
