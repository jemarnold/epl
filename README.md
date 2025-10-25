
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{epl}` package

<!-- badges: start -->

<!-- badges: end -->

`{epl}` is a repository of functions and scripts for common tasks for
use by students in the UBC Environmental Physiology Laboratory.

## Online App

`<under development>`

## Installation

You can install the development version of `{epl}` from
[GitHub](https://github.com/jemarnold/epl) with:

``` r
# install.packages("remotes")
remotes::install_github("jemarnold/epl")
```

## Usage

### `read_tymewear()`

This function will read *“Live Processed CSV”* or *“Post Processed CSV”*
files exported from *Tymewear VitalPro* and return a list of two data
frames with recorded data and file details.

`tyme$data` contains the recorded raw data samples for breathing rate,
tidal volume, and ventilation: `c("br", "vt", "ve")`, with time in
seconds and timestamps in `datetime` format.

`tyme$details` contains the file details & metadata, including
`c("gender", "weight", "date", "app-start-time")`.

``` r
## load {epl} package
library(epl)

## specify the file path via any number of methods
## this gives an example format
file_path <- file.path("root_project_folder", "data_folder", "my_file.csv")
file_path
#> [1] "root_project_folder/data_folder/my_file.csv"

## example files are available by calling `example_epl()`
file_path <- example_epl("tymewear_live")

tymelive <- read_tymewear(file_path)
tymelive
#> $data
#> # A tibble: 1,083 × 5
#>     time timestamp              br    vt    ve
#>    <dbl> <dttm>              <dbl> <dbl> <dbl>
#>  1  0    2025-10-23 10:50:11   127   135    17
#>  2  4.14 2025-10-23 10:50:15   149   135    20
#>  3  7.95 2025-10-23 10:50:18   150   105    16
#>  4 13.0  2025-10-23 10:50:24   138   240    33
#>  5 17.2  2025-10-23 10:50:28   133   111    15
#>  6 21.2  2025-10-23 10:50:32   128   163    21
#>  7 25.7  2025-10-23 10:50:36   130   361    47
#>  8 33.9  2025-10-23 10:50:44   119   588    70
#>  9 39.3  2025-10-23 10:50:50    80   114     9
#> 10 44.7  2025-10-23 10:50:55   100   221    22
#> # ℹ 1,073 more rows
#> 
#> $details
#> # A tibble: 60 × 2
#>    ...1          ...2    
#>    <chr>         <chr>   
#>  1 Info Section  ""      
#>  2 type          "0"     
#>  3 stages        "[]"    
#>  4 gender        "Female"
#>  5 weight        "64.0"  
#>  6 weight_units  "SI"    
#>  7 activity-name "TW02"  
#>  8 f_v           "1.0"   
#>  9 activity-type "0"     
#> 10 sport         "2"     
#> # ℹ 50 more rows
```

### `read_parvo()`

This function will read *.CSV* or *.xlsx* (but not *.XLS*) files
exported by *Parvo Medics TrueOne 2400* and return a list of three data
frames with recorded data, file details, and events.

`parvo$data` contains the recorded raw data samples for exported data
channels (e.g. `c("VO2", "VCO2", "RR", "Vt")`) and calculated metabolic
values (e.g. `c("O2kJ", "O2kcal", "Paer", "METS")`; see `?read_parvo`
for details on calculated values).

`parvo$details` contains the file details & metadata, including
`c("Date", "Name", "Sex", "Age")` (all metric values).

`parvo$events` contains any manually entered events with `"TIME"` in
seconds and event descriptions.

``` r
file_path <- example_epl("parvo_binned")

parvo <- read_parvo(file_path)
parvo
#> $data
#> # A tibble: 179 × 25
#>     TIME    HR VO2kg   VO2  VCO2   RER    RR    Vt    VE VEVO2 VEVCO2  FEO2
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
#>  1  18.1     0  5.69 0.404 0.419 1.04   13.3 1.11  12.1   36.5   35.2  17.6
#>  2  31.5     0  6.80 0.483 0.484 1.00   17.8 0.993 14.5   36.7   36.6  17.6
#>  3  49.5   107  6.08 0.432 0.448 1.04   10.0 1.66  13.6   38.4   37.0  17.7
#>  4  62.5   107  4.06 0.288 0.321 1.11   13.8 0.816  9.27  39.2   35.2  17.7
#>  5  77.5   107  8.61 0.611 0.648 1.06   16.0 1.28  16.8   33.4   31.5  17.2
#>  6  94.9   107  7.52 0.534 0.528 0.990  13.8 1.15  13.0   29.8   30.1  16.8
#>  7 107.    107  6.80 0.483 0.514 1.06   15.0 1.10  13.6   34.2   32.1  17.3
#>  8 123.    107  5.94 0.422 0.454 1.08   11.2 1.33  12.2   35.2   32.7  17.4
#>  9 138.    107  2.47 0.175 0.186 1.06   16.0 0.392  5.13  35.7   33.6  17.5
#> 10 153.    107  3.92 0.278 0.294 1.06   12.1 0.845  8.42  36.9   34.9  17.6
#> # ℹ 169 more rows
#> # ℹ 13 more variables: FECO2 <dbl>, FATmin <dbl>, CHOmin <dbl>, Breath <dbl>,
#> #   FatOx <dbl>, CarbOx <dbl>, O2kJ <dbl>, O2kcal <dbl>, O2work <dbl>,
#> #   O2energy <dbl>, O2power <dbl>, O2pulse <dbl>, METS <dbl>
#> 
#> $details
#> # A tibble: 1 × 16
#>   Date             Name  Sex     Age Height Weight `Insp. temp` `Baro. pressure`
#>   <chr>            <chr> <chr> <dbl>  <dbl>  <dbl>        <dbl>            <dbl>
#> 1 2025-10-23 10:5… TW02  Male      0    176     71         23.6             755.
#> # ℹ 8 more variables: `Insp. humidity` <dbl>, `STPD to BTPS` <dbl>,
#> #   `O2 Gain` <dbl>, `CO2 Gain` <dbl>, `Base O2` <dbl>, `Base CO2` <dbl>,
#> #   `Measured O2` <dbl>, `Measured CO2` <dbl>
#> 
#> $events
#> # A tibble: 23 × 2
#>     TIME Events        
#>    <dbl> <chr>         
#>  1   240 Start Exercise
#>  2   240 UP1 265W      
#>  3   540 Stop Exercise 
#>  4   540 Cadence 95    
#>  5   900 Start Exercise
#>  6   900 RP2 265W      
#>  7  1200 Stop Exercise 
#>  8  1200 RPE 6-20 15   
#>  9  1200 Cadence 95    
#> 10  1440 Start Exercise
#> # ℹ 13 more rows
```

`read_parvo()` has an argument to add timestamps in `datetime` format to
`parvo$data`, taken from `parvo$details` file start time. This is
recorded from system time to the nearest whole second (i.e. precise to ±
0.5 sec, assuming system time is accurate).

``` r
## timestamps can be added
read_parvo(file_path, add_timestamp = TRUE)$data
#> # A tibble: 179 × 26
#>     TIME timestamp              HR VO2kg   VO2  VCO2   RER    RR    Vt    VE
#>    <dbl> <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  18.1 2025-10-23 10:50:23     0  5.69 0.404 0.419 1.04   13.3 1.11  12.1 
#>  2  31.5 2025-10-23 10:50:36     0  6.80 0.483 0.484 1.00   17.8 0.993 14.5 
#>  3  49.5 2025-10-23 10:50:54   107  6.08 0.432 0.448 1.04   10.0 1.66  13.6 
#>  4  62.5 2025-10-23 10:51:07   107  4.06 0.288 0.321 1.11   13.8 0.816  9.27
#>  5  77.5 2025-10-23 10:51:22   107  8.61 0.611 0.648 1.06   16.0 1.28  16.8 
#>  6  94.9 2025-10-23 10:51:39   107  7.52 0.534 0.528 0.990  13.8 1.15  13.0 
#>  7 107.  2025-10-23 10:51:51   107  6.80 0.483 0.514 1.06   15.0 1.10  13.6 
#>  8 123.  2025-10-23 10:52:08   107  5.94 0.422 0.454 1.08   11.2 1.33  12.2 
#>  9 138.  2025-10-23 10:52:23   107  2.47 0.175 0.186 1.06   16.0 0.392  5.13
#> 10 153.  2025-10-23 10:52:37   107  3.92 0.278 0.294 1.06   12.1 0.845  8.42
#> # ℹ 169 more rows
#> # ℹ 16 more variables: VEVO2 <dbl>, VEVCO2 <dbl>, FEO2 <dbl>, FECO2 <dbl>,
#> #   FATmin <dbl>, CHOmin <dbl>, Breath <dbl>, FatOx <dbl>, CarbOx <dbl>,
#> #   O2kJ <dbl>, O2kcal <dbl>, O2work <dbl>, O2energy <dbl>, O2power <dbl>,
#> #   O2pulse <dbl>, METS <dbl>
```

### Helper Functions

`<under development>`

#### `example_epl()`

This function can be used to retrieve example files included in {epl} to
test processing functions.

``` r
## calling `example_epl()` will return a list of all included example files
example_epl()
#> [1] "parvo_binned.CSV"  "parvo_bxb.CSV"     "tymewear_live.csv"
#> [4] "tymewear_post.csv"

## partial matching will error if matches multiple
example_epl("tymewear")
#> Error in `example_epl()`:
#> ! Multiple files match "tymewear":
#> ℹ Matching files: "tymewear_live.csv" and "tymewear_post.csv"

## calling a specific file by name will return the file path to that file
## partial string matching works for uniquely identifiable file names
example_epl("tymewear_live")
#> [1] "C:/Users/Jem/AppData/Local/Temp/Rtmpy8S1BW/temp_libpath2498217936ab/epl/extdata/tymewear_live.csv"
```

## To do

- Update `read_tymewear()` method for *“tymepost*” export file type.

- Add local outlier filtering for metabolic data.

- Add mean peak value detection, i.e. for V̇O<sub>2</sub>peak.

- Add digital filtering methods (e.g. Butterworth, smoothing spline,
  simple moving average).

- Add template display theme for `ggplot2` plotting.

- Add 4-parameter monoexponential curve fitting via `nls()`
  self-starting functions.
