flextable_tooltip <- function(x){
  ft <- flextable(x)
  ft <- theme_vader(ft)
  ft <- set_table_properties(ft, layout = "autofit")
  ft
}
