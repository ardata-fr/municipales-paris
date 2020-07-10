as_secteur <- function(x){
  secteur <- c("Paris centre",
               "5e arrondissement",
               "6e arrondissement",
               "7e arrondissement",
               "8e arrondissement",
               "9e arrondissement",
               "10e arrondissement",
               "11e arrondissement",
               "12e arrondissement",
               "13e arrondissement",
               "14e arrondissement",
               "15e arrondissement",
               "16e arrondissement",
               "17e arrondissement",
               "18e arrondissement",
               "19e arrondissement",
               "20e arrondissement")
  cut(x,
      breaks = c(-1, seq(4.5, 20.5, by = 1)),
      labels = secteur)
  
}
