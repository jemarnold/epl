#' @name parvo_binned
#'
#' @title Parvo 15-sec binned export of cycling interval workout
#'
#' @description A .CSV file exported from Parvo Medics TrueOne 2400 metabolic
#'   cart with 15-sec binned average ventilation and gas exchange data for a
#'   5x 5-minute cycling interval workout.
#'
#' @docType data
#'
#' @format A raw .CSV file with metadata and a primary data table with 16
#'   columns and 179 rows.
#'
#' @details
#' Session records a 5x 5-min cycling interval workout with the participant
#'   alternating body position between upright training position (*"UP"*) and
#'   aerodynamic racing position (*"RP"*) each work bout. Work bout Borg RPE
#'   were between 14-15.
#'
#' This session was recorded simultaneously with both Parvo and Tymewear. The
#'   Parvo file start time is accurate to within 1 sec (± 0.5 sec precision).
#'   However, the Tymewear timestamps are not all accurate, and are inconsistent
#'   due to a current issue in the recording app.
#'
#' Interval start and stop events are recorded, along with manually identified
#'   cadence and participant-reported Borg RPE.
#'
#' Heart rate data are not valid.
#'
NULL




#' @name parvo_bxb
#'
#' @title Parvo breath-by-breath export of cycling interval workout
#'
#' @description A .CSV file exported from Parvo Medics TrueOne 2400 metabolic
#'   cart with breath-by-breath ventilation and gas exchange data for a
#'   5x 5-minute cycling interval workout.
#'
#' @docType data
#'
#' @format A raw .CSV file with metadata and a primary data table with 16
#'   columns and 1056 rows.
#'
#' @details
#' Session records a 5x 5-min cycling interval workout with the participant
#'   alternating body position between upright training position (*"UP"*) and
#'   aerodynamic racing position (*"RP"*) each work bout. Work bout Borg RPE
#'   were between 14-15.
#'
#' This session was recorded simultaneously with both Parvo and Tymewear. The
#'   Parvo file start time is accurate to within 1 sec (± 0.5 sec precision).
#'   However, the Tymewear timestamps are not all accurate, and are inconsistent
#'   due to a current issue in the recording app.
#'
#' Interval start and stop events are recorded, along with manually identified
#'   cadence and participant-reported Borg RPE.
#'
#' Parvo uses a mixing chamber and does not correct for time delay between
#'   when ventilation is recorded at the Pneumotach per breath, and when the
#'   gas exchange from that breath is recorded by the sample line. Therefore,
#'   it is *ONLY* valid to use raw ventilation values exported as
#'   breath-by-breath. Breath-by-breath gas exchange values are *NOT* valid
#'   without additional correction processing.
#'
#' Heart rate data are not valid.
#'
NULL




#' @name parvo_excel
#'
#' @title Parvo 30-sec binned export of maximal graded cycling assessment
#'
#' @description An .xlsx file exported from Parvo Medics TrueOne 2400 metabolic
#'   cart with 30-sec binned average ventilation, gas exchange, and cycling
#'   work rate & cadence data for a maximal incremental ramp to V̇O~2~peak.
#'
#' @docType data
#'
#' @format A raw .xlsx file with metadata and a primary data table with 20
#'   columns and 55 rows.
#'
#' @details
#' Session records a maximal incremental ramp to V̇O~2~. Protocol details are
#'   unknown. Includes work rate (*"WorkR"*) and cadence (*"RPM"*).
#'
NULL




#' @name parvo_ramp
#'
#' @title Parvo 30-sec binned export of maximal graded cycling assessment
#'
#' @description A .CSV file exported from Parvo Medics TrueOne 2400 metabolic
#'   cart with 30-sec binned average ventilation and gas exchange data for a
#'   submaximal graded step test and maximal incremental ramp to V̇O~2~peak.
#'
#' @docType data
#'
#' @format A raw .CSV file with metadata and a primary data table with 15
#'   columns and 91 rows.
#'
#' @details
#' Session records a submaximal graded step test starting at 100 W and
#'   increasing by 50 W per 5-min stage to maximum task tolerance. Followed
#'   by a maximal incremental ramp to V̇O~2~.
#'
#' Heart rate data are not valid.
#'
NULL




#' @name tymewear_live
#'
#' @title Tymewear breath-by-breath sample export of cycling interval workout
#'
#' @description A .csv file exported from Tymewear VitalPro chest strap with
#'   breath-by-breath export of ventilation data for a 5x 5-minute cycling
#'   interval workout.
#'
#' @docType data
#'
#' @format A raw .csv file with metadata and a primary data table with 9
#'   columns and 1083 rows.
#'
#' @details
#' Session records a 5x 5-min cycling interval workout with the participant
#'   alternating body position between upright training position (*"UP"*) and
#'   aerodynamic racing position (*"RP"*) each work bout. Work bout Borg RPE
#'   were between 14-15.
#'
#' This session was recorded simultaneously with both Parvo and Tymewear. The
#'   Parvo file start time is accurate to within 1 sec (± 0.5 sec precision).
#'   The Tymewear "live" sample timestamps accurate, however other timestamps
#'   in the "live" and "post" file metadata are *NOT* accurate due to a
#'   current issue in the recording app.
#'
#' Heart rate, cycling power, cadence, and speed are exported from the Monarch
#'   cycle ergometer in the Tymewear "live" file in wide format at the bottom
#'   of the file.
#'
NULL




#' @name tymewear_post
#'
#' @title Tymewear time series and breath-by-breath sample export of cycling
#'   interval workout
#'
#' @description A .csv file exported from Tymewear VitalPro chest strap with
#'   1 Hz time series and breath-by-breath export of ventilation data for a
#'   5x 5-minute cycling interval workout.
#'
#' @docType data
#'
#' @format A raw .csv file with metadata and a primary data table with 31
#'   columns and 2942 rows.
#'
#' @details
#' Session records a 5x 5-min cycling interval workout with the participant
#'   alternating body position between upright training position (*"UP"*) and
#'   aerodynamic racing position (*"RP"*) each work bout. Work bout Borg RPE
#'   were between 14-15.
#'
#' This session was recorded simultaneously with both Parvo and Tymewear. The
#'   Parvo file start time is accurate to within 1 sec (± 0.5 sec precision).
#'   The Tymewear "post" start time is *NOT* accurate due to a current issue
#'   in the recording app. Sample timestamps in the Tymewear "live" file are
#'   accurate.
#'
#' The Tymewear "post" file contains two data tables: on the right is time
#'   series data for the ventilatory and performance variables exported at
#'   1 Hz (1 second samples). On the left is breath-by-breath samples for the
#'   ventilation data.
#'
#' Heart rate, cycling power, cadence, and speed are exported from the Monarch
#'   cycle ergometer at 1 Hz in the time series columns.
#'
NULL
