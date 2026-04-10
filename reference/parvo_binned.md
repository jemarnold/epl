# Parvo 15-sec binned export of cycling interval workout

A .CSV file exported from Parvo Medics TrueOne 2400 metabolic cart with
15-sec binned average ventilation and gas exchange data for a 5x
5-minute cycling interval workout.

## Format

A raw .CSV file with metadata and a primary data table with 16 columns
and 179 rows.

## Details

Session records a 5x 5-min cycling interval workout with the participant
alternating body position between upright training position (*"UP"*) and
aerodynamic racing position (*"RP"*) each work bout. Work bout Borg RPE
were between 14-15.

This session was recorded simultaneously with both Parvo and Tymewear.
The Parvo file start time is accurate to within 1 sec (± 0.5 sec
precision). However, the Tymewear timestamps are not all accurate, and
are inconsistent due to a current issue in the recording app.

Interval start and stop events are recorded, along with manually
identified cadence and participant-reported Borg RPE.

Heart rate data are not valid.
