summary_data <- function(scores){
  summ_detail <- scores[, list(score = sum(score)), by = c("TOUR", "nuance", "lib_nuance")]
  summ_global <- scores[, list(NB_EXPRIM = sum(score)), by = c("TOUR")]
  summ_detail <- merge(summ_detail, summ_global, by = c("TOUR"))
  summ_detail[, c("score_pct", "x") := list(score / NB_EXPRIM, "B")]
  summ_detail[, c("NB_EXPRIM") := list(NULL)]
  summ_global[, c("score_pct", "x", "nuance", "lib_nuance") := list(1.0, "A", NA_character_, NA_character_)]
  setnames(summ_global, old = "NB_EXPRIM", new = "score")
  summ_detail <- rbind(summ_detail, summ_global)
  summ_detail[, tooltip := glue_data(.SD, "{lib_nuance}:\n{score} voix\n{round(score_pct*100, 2)}% des voix")]
  setDF(summ_detail)
  summ_detail
}
