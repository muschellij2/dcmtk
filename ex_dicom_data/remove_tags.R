##################################################
# Making DICOM tables
##################################################
rm(list = ls())
library(dplyr)
library(rvest)
library(httr)
library(xml2)
library(tidyr)
tags = read.delim2("removal_tags.txt", as.is = TRUE)

tags$Tag = gsub(" ", "", tags$Tag)
tags$Tag = paste0("(", tags$Tag, ")")
tags$Action = NULL

colnames(tags) = c("tag", "name")
removal_tags = tags
removal_tags = removal_tags %>% filter(!grepl("^\\(000[0|2]", tag))

save(removal_tags,
     file = "data/removal_tags.rda",
     compression_level = 9)

url = "http://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/DICOM.html"
res = GET(url)
info = content(res)

##################################################
# read in extensive table
##################################################
tab = read_html("DICOMLookup.htm")
tab = html_nodes(tab, "table")
tab = html_table(tab)[[1]]
colnames(tab) = trimws(tab[1,])
tab = tab[-1,]

tab$Tag = gsub("\\s", "", tab$Tag)
no_end = grep("[^\\)]$", tab$Tag)
tab$Tag[no_end] = paste0(tab$Tag[no_end], ")")

tab = tab %>%
  mutate(
    Attribute = trimws(Attribute),
    Attribute = gsub("^>*", "", Attribute),
    Attribute = gsub("\\s+", " ", Attribute),
    Attribute = trimws(Attribute),
    Attribute = tolower(Attribute)
  ) %>%
  arrange(Tag, Attribute)

utab = tab %>%
  select(Tag, Attribute) %>%
  unique

utab = utab %>%
  group_by(Tag) %>%
  mutate(n = seq(n()),
         max_n = max(n))
z = spread(utab, n, value = Attribute)

###### all of these are the same
utab %>%
  filter(max_n > 1) %>%
  data.frame

utab = tab %>%
  select(Tag, Attribute) %>%
  unique %>%
  group_by(Tag) %>%
  slice(1) %>%
  ungroup %>%
  mutate(Attribute = gsub("date and time", "datetime",
              Attribute)
  )

############################
# Date tags only
############################
date_tags = utab %>%
  filter(grepl("date", Attribute),
         !grepl("datetime", Attribute)) %>%
  data.frame
colnames(date_tags) = c("tag", "name")
save(date_tags,
     file = "data/date_tags.rda",
     compression_level = 9)


############################
# Datetime tags only
############################
datetime_tags = utab %>%
  filter(grepl("datetime", Attribute)) %>%
  data.frame
colnames(datetime_tags) = c("tag", "name")
save(datetime_tags,
     file = "data/datetime_tags.rda",
     compression_level = 9)



# time_tags = utab %>%
#   filter(grepl("time", Attribute),
#          !grepl("datetime", Attribute)) %>%
#   data.frame
#
# good_times = c(
#   "repetition time", "echo time", "inversion time",
#   "exposure time", "tomo time",
#   "trigger time",
#   "frame time",
#   "radiopharmaceutical start time",
#   "radiopharmaceutical stop time",
#   "time source", "time distribution protocol",
#   "exposure time in âµs", "time of flight contrast",
#   "time domain filtering", "inversion times", "effective echo time",
#   "trigger delay time", "channel time skew",
#   "referenced time offsets", "time slot vector", "number of time slots",
#   "time slot information sequence", "time slot time", "time slice vector",
#   "number of time slices", "dead time factor",
#   "dead time correction flag",
#   "phototimer setting",
#   "total time",
#   "trigger time offset",
#   "multiplex group time offset",
#   "contrast/bolus stop time",
#   "contrast/bolus stop time",
#   "number of event timers")
# time_tags = time_tags %>% filter(!Attribute %in% good_times)

