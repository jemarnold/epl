# Read Tymewear Exported CSV Data

Read *"Live Processed CSV"* or *"Post Processed CSV"* files exported
from *Tymewear VitalPro* and return a list of two data frames with
recorded data and file details

## Usage

``` r
read_tymewear(file_path, ...)

# S3 method for class 'live'
read_tymewear(file_path, ...)

# S3 method for class 'post'
read_tymewear(file_path, ...)
```

## Arguments

- file_path:

  The file path as a character string, including *.csv* file type.

- ...:

  Additional arguments passed to `read_tymewear`.

## Value

A list with two
[tibbles](https://tibble.tidyverse.org/reference/tibble-package.html).

- `tymelive$data` contains the data table.

- `tymelive$details` contains the file metadata.

## Details

This generic function detects *Tymewear* exported file type by searching
the top of the file for recognisable column data belonging to either
*"tymewear.live"* or *"tymewear.post"*, and will read and import the
resulting file.

## Examples

``` r
## retrieve example tymewear file
file_path <- example_epl("tymewear_live")

tyme <- read_tymewear(file_path)
tyme
#> $data
#> # A tibble: 1,083 × 5
#>     time timestamp              br    vt    ve
#>    <dbl> <dttm>              <dbl> <dbl> <dbl>
#>  1  0    2025-10-23 10:50:11  12.7  1.35    17
#>  2  4.14 2025-10-23 10:50:15  14.9  1.35    20
#>  3  7.95 2025-10-23 10:50:18  15    1.05    16
#>  4 13.0  2025-10-23 10:50:24  13.8  2.4     33
#>  5 17.2  2025-10-23 10:50:28  13.3  1.11    15
#>  6 21.2  2025-10-23 10:50:32  12.8  1.63    21
#>  7 25.7  2025-10-23 10:50:36  13    3.61    47
#>  8 33.9  2025-10-23 10:50:44  11.9  5.88    70
#>  9 39.3  2025-10-23 10:50:50   8    1.14     9
#> 10 44.7  2025-10-23 10:50:55  10    2.21    22
#> # ℹ 1,073 more rows
#> 
#> $details
#> # A tibble: 60 × 2
#>    parameter     value 
#>    <chr>         <chr> 
#>  1 Info Section  ""    
#>  2 type          "0"   
#>  3 stages        "[]"  
#>  4 gender        "Male"
#>  5 weight        "71.0"
#>  6 weight_units  "SI"  
#>  7 activity-name "TW02"
#>  8 f_v           "1.0" 
#>  9 activity-type "0"   
#> 10 sport         "2"   
#> # ℹ 50 more rows
#> 
```
