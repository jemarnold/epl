# Read Parvo Exported CSV Data

Read *.CSV* or *.xlsx* (but not *.XLS*) files exported by *Parvo Medics
TrueOne 2400* and return a list of three data frames with recorded data,
file details, and events.

## Usage

``` r
read_parvo(file_path, add_timestamp = FALSE, ...)
```

## Arguments

- file_path:

  The file path as a character string, including *.csv* or *.xlsx* file
  type.

- add_timestamp:

  A logical to add a "timestamp" column to the data table with date-time
  values, useful for synchronisation with other recordings by time of
  day. Precise to ± 0.5 seconds.

- ...:

  Additional arguments (not currently used).

## Value

A list with three
[tibbles](https://tibble.tidyverse.org/reference/tibble-package.html).

- `parvo$data` contains the data table.

- `parvo$details` contains the file metadata.

- `parvo$events` contains manual event inputs.

## Details

This function can only parse *.CSV* files exported directly from a Parvo
metabolic cart. *.XLS* exported from Parvo are obsolete and the file
format cannot be read. They must be re-saved as *.xlsx* before reading
with this function.

Data from all exported channels (e.g. `c("VO2", "VCO2", "Vt")`) will be
exported as-is.

Additional data columns will be calculated if the required exported data
are present. All energetic calculations are derived from *Peronnet &
Massicotte, 1991. Table of nonprotein respiratory quotient: an update*.

These include:

- `FatOx` and `CarbOx` are respective substrate oxidation rates in
  g/min.

- `O2kJ` and `O2kcal` are energy equivalents of oxygen in kJ/L and
  kcal/L (kilojoules and kilocalories per litre `VO2`), respectively.

- `O2work`, `O2power`, and `O2energy` are aerobic metabolic work
  expenditure, energy expenditure, and power output in kJ/min, kcal/min,
  and W (kilojoules per minute, kilocalories per minute, and joules per
  second), respectively.

- `O2pulse` is a ratio of `VO2` to heart rate (`HR`) in ml/min/bpm
  (millilitres of `VO2` per heart beat).

- `Economy` is a ratio of the oxygen cost of work, in W/L/min (external
  power output in watts per litre per minute of `VO2`)

- `GE` (gross efficiency) is a ratio of external work to internal
  metabolic work, as a percent, accounting for `VO2` and substrate
  oxidation (`RER`).

- `METS` (metabolic equivalent of task) is a deprecated method of
  estimating the oxygen cost of common physically active tasks adjusted
  for body mass, relative to resting metabolic rate (approximately 3.5
  mL/kg/min), calculated as `VO2kg / 3.5`.

## Examples

``` r
## retrieve example parvo file
file_path <- example_epl("parvo_binned")

parvo <- read_parvo(file_path, add_timestamp = TRUE)
parvo
#> $data
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
#> 
```
