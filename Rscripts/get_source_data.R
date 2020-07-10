library(stringi)
library(stringr)
library(data.table)
library(dplyr)
library(sf)
library(flextable)
library(glue)

source("Rscripts/paris_arr.R")

source("Rscripts/as_secteur.R")
source("Rscripts/summary_data.R")
source("Rscripts/flextable_tooltip.R")


if(!dir.exists("data-raw")){
  
  library(crrri)
  library(xml2)
  library(processx)
  
  # tour 1 -----
  url <- "https://parisdata.opendatasoft.com/explore/dataset/elections-municipales-2020-1ertour/information/"  
  outer_html <- run(find_chrome_binary(), args = c("--headless", "--dump-dom", url))
  outtemp <- tempfile(fileext = ".html")
  cat(outer_html$stdout, file = outtemp)
  
  files_tour_1 <- read_html(outtemp) %>% 
    xml_find_all('//*[@class="ods-dataset-attachments__attachment-link ods-button ng-isolate-scope"]') %>% 
    xml_attr("href")
  
  unlink(outtemp)
  
  # tour 2 -----
  url <- "https://parisdata.opendatasoft.com/explore/dataset/elections-municipales-2020-2emetour/information/"  
  outer_html <- run(find_chrome_binary(), args = c("--headless", "--dump-dom", url))
  outtemp <- tempfile(fileext = ".html")
  cat(outer_html$stdout, file = outtemp)
  
  files_tour_2 <- read_html(outtemp) %>% 
    xml_find_all('//*[@class="ods-dataset-attachments__attachment-link ods-button ng-isolate-scope"]') %>% 
    xml_attr("href")
  
  unlink(outtemp)
  urls <- c(files_tour_1, files_tour_2)
  urls <- paste0("https://parisdata.opendatasoft.com", urls)
  dir.create("data-raw",  showWarnings = FALSE)
  for(i in urls){
    dest <- file.path("data-raw", basename(i))
    curl::curl_download(url = i, destfile = dest)
  }
}



# read all files -----
data_raw <- lapply(list.files("data-raw", full.names = TRUE), function(x){
  readxl::read_xls(x)# some warnings but we can live with them
})

# organize the datasets ----
datacolnames <- Reduce(intersect, lapply(data_raw, colnames) )
nb_cols <- datacolnames[grepl("^NB_", datacolnames)]
id_cols <- setdiff(datacolnames, nb_cols)

data_scores <- lapply(data_raw, function(x){
  melt(as.data.table(x),
       id.vars = id_cols,
       measure.vars = setdiff(colnames(x), datacolnames),
       value.name = "score",
       variable.name = "candidat")
})

# data_scores: the scores ----
data_scores <- rbindlist(data_scores, use.names = TRUE)
data_scores[, candidat := gsub("^(M\\.|Mme)[ ]+", "", candidat)]
data_scores[, candidat := str_to_upper(candidat)]
data_scores[, candidat := stri_trans_general(candidat, "latin-ascii")]
data_scores[, candidat := gsub("BUZIN AGNES", "BUZYN AGNES", candidat)]

# data_scores: participation, registered people, ... ----
data_cpt <- lapply(data_raw, function(x){
  x[, c(id_cols, nb_cols)]
})
data_cpt <- rbindlist(data_cpt, use.names = TRUE)

# political colours ----
col_pol <- readxl::read_xlsx("data-ref/couleurs_pol.xlsx")
setDT(col_pol)
setnames(col_pol, old = c("Code du département", 
                          "Libellé du département", "Code commune", 
                          "Libellé commune", "Libellé abrégé liste", 
                          "Nuance Liste", "Sexe candidat", 
                          "Nom candidat", "Prénom candidat"),
         new = c("dep_code", "dep_lib", "com_code", "com_lib", "liste_lib", "nuance", "genre",
                 "nom", "prenom"))
col_pol <- col_pol[, c("dep_code", "dep_lib", "com_code", "com_lib", "liste_lib", "nuance", "genre",
                       "nom", "prenom")]
col_pol[, candidat := paste(nom, prenom)]
col_pol[, candidat := str_to_upper(candidat)]
col_pol[, candidat := stri_trans_general(candidat, "latin-ascii")]
col_pol <- col_pol[,c("candidat", "nuance"), with = FALSE]

# references for political parties ----
pol_references <- read.csv("data-ref/codes_nuances_2020.csv", sep = ";", stringsAsFactors = FALSE)
setDT(pol_references)
setorderv(pol_references, "NumOrdNua")
lev_nuances <- pol_references$CodNua
lev_lib_nuances <- pol_references$LibNua

pol_references[, CodNua := factor(NumOrdNua, levels = NumOrdNua, labels = CodNua)]
pol_references[, NumOrdNua := NULL]
pol_references[, Definition := NULL]
setnames(pol_references, 
         old = c("Bloc", "CodNua", "LibNua"), 
         new = c("couleur_pol", "nuance", "lib_nuance"))
col_pol <- merge(col_pol, pol_references, by = "nuance", all.x = TRUE)

# add Paris'sector dimension ----
data_cpt[, secteur := as_secteur(NUM_ARROND)]
data_scores[, secteur := as_secteur(NUM_ARROND)]

NB_EXPRIM <- data_cpt[, list(NB_EXPRIM = sum(NB_EXPRIM, na.rm = TRUE)), by = c("TOUR", "secteur")]

scores <- merge(data_scores, col_pol, by = "candidat", all.x = FALSE, all.y = FALSE)
scores <- scores[, list(score = sum(score, na.rm = TRUE)), by = c("TOUR", "candidat", "secteur", "nuance")]

setorder(scores, TOUR, secteur, -score)
scores <- merge(scores, NB_EXPRIM, by = c("TOUR", "secteur"))
scores[, score_pct := score / NB_EXPRIM]
scores[, NB_EXPRIM := NULL]

nuances <- unique(col_pol[, c("nuance", "couleur_pol", "lib_nuance"), with = FALSE])

scores <- merge(scores, nuances, by = "nuance")
scores[ , nuance := factor(nuance, levels = lev_nuances, labels = lev_nuances)]
scores[ , lib_nuance := factor(lib_nuance, levels = lev_lib_nuances, labels = lev_lib_nuances)]

# for scales -----
nuances_col <- unique(col_pol[, c("lib_nuance", "nuance", "fill"), with = FALSE])

# summ_detail for barplot ----
summ_detail <- summary_data(scores)

dat_summary <- scores[,list(score = sum(score)), by = c("secteur", "nuance", "candidat", "TOUR")]
setorder(dat_summary, secteur, -score)

# base flextable for tooltips on maps ----
flextables_tour_secteur <- dat_summary[, list(ft = list(flextable_tooltip(.SD))), c("TOUR", "secteur")]

scores <- left_join(paris_arr, scores, by = "secteur")

setDF(flextables_tour_secteur)
setDF(summ_detail)
setDF(dat_summary)
setDF(col_pol)
setDF(data_scores)
setDF(data_cpt)
setDF(scores)
setDF(nuances)
setDF(nuances_col)

saveRDS(nuances_col, file = "data/nuances_col.RDS")
saveRDS(summ_detail, file = "data/summ_detail.RDS")
saveRDS(dat_summary, file = "data/dat_summary.RDS")
saveRDS(flextables_tour_secteur, file = "data/flextables_tour_secteur.RDS")
saveRDS(scores, file = "data/scores.RDS")
saveRDS(col_pol, file = "data/col_pol.RDS")

remove(list = ls())

