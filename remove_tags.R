rm(list = ls())
library(dplyr)
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