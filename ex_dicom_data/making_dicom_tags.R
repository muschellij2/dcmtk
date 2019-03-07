##################################################
# Making DICOM tables
##################################################
rm(list = ls())
library(dplyr)
library(rvest)
library(httr)
library(xml2)
library(tidyr)
library(data.table)
library(tools)


files = c("ex_dicom_data/id_dicom_dump.html",
          "ex_dicom_data/file_dicom_dump.html",
          "ex_dicom_data/large_dicom_dump.html")
file = files[3]
tabs = lapply(files, function(file){

  tab = read_html(file)
  tables = html_nodes(tab, "table")
  tab = html_table(tables)
  tab = tab[[2]]
  if (basename(file) == "large_dicom_dump.html") {
    hd = tab[1,,]
    tab = tab[-1,]
    hd = unlist(hd)
  } else {
    hd = colnames(tab)
  }
  miss = hd == "" | is.na(hd)
  stopifnot(sum(miss) == 1)
  hd[miss] = "retired"
  colnames(tab) = tolower(trimws(hd))

  dicom_tags = tab
})

tabs = lapply(tabs, function(x){
  x$vm = as.character(x$vm)
  return(x)
})

dicom_tags = rbindlist(tabs)
dicom_tags = as.data.frame(dicom_tags)
dicom_tags = dicom_tags %>% arrange(tag, name)


sub_weird_space = function(x) {
  x = gsub("\xc2\xa0", " ", x)
  x = gsub("\xe2\x80\x8b", " ", x)
  x = gsub("\xc2\xb5", " ", x)
  x = gsub("\\s+", " ", x)
  x
}
dicom_tags = dicom_tags %>%
  mutate(name = sub_weird_space(name),
         keyword = sub_weird_space(keyword),
         keywork = gsub(" ", "", keyword),
         retired = ifelse(is.na(retired), "", retired))

dicom_tags$tag = tolower(dicom_tags$tag)

stopifnot(!any(duplicated(dicom_tags$tag)))
save(dicom_tags,
     file = "data/dicom_tags.rda",
     compression_level = 9)
