# library(healthcareai)
library(lubridate)
library(purrr)

d_dates <- data.frame(a_DTS = "2018-3-25",
                      b_DTS = "2018-03-25",
                      c_DTS = "3-25-2018",
                      d_DTS = "03-25-2018",
                      e_DTS = "Mar 25 2018",
                      f_DTS = "March 25th, 2018",
                      not_DTS = "string cheese",
                      aa_DTS = "2018-3-25 22:30:00",
                      bb_DTS = "2018-03-25 22:30:00",
                      cc_DTS = "3-25-2018 22:30:00",
                      dd_DTS = "03-25-2018 22:30:00",
                      ee_DTS = "Mar 25 2018 22:30:00",
                      ff_DTS = "March 25th, 2018 22:30:00")

date_formats <- map(slice(d_dates, 1),
                    guess_formats,
                    orders = c("ymd", "mdy", "ymd HMS", "mdy HMS"))


# Find working formats
valid_formats <- map2(slice(d_dates, 1), date_formats, function(x,y) {
  temp <- map2(x, y, function(x,y) {
    as.POSIXct(x = x, format = y)})
  # Can't use is.POSIXct here because NA has class POSIXct. wtf.
  unlist(map(temp, function(x) {!is.na(x)}))
})

# Collapse to one format and find columns with no valid format
valid_formats <- map_int(valid_formats, function(x) {
  x <- match(TRUE, x)
})
bad_date_cols <- names(valid_formats[is.na(valid_formats)])

# Remove formats that don't work
d_dates <- d_dates %>% select(-one_of(bad_date_cols))
date_formats <- date_formats[!is.na(valid_formats)]
valid_formats <- valid_formats[!is.na(valid_formats)]

# Working formats
use_formats <- map2_chr(date_formats, valid_formats, `[[`)

# Convert dates
d_dates <- map2_df(d_dates, use_formats, function(x,y) {
  as.POSIXct(x = x, format = y)
})

return()



prep_data(d_dates)















