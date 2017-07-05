resultats <- function( x ){
  x <- str_to_lower(x)

  bac2017 %>%
    filter( str_detect(str_to_lower(Nom), x) ) %>%
    mutate( group = as.numeric(Resultat)) %>%
    summarise(
      rattrapage  = 100 * sum(group == 1 ) / n() ,
      admis       = 100 * sum(group >= 2 ) / n(),
      mention_ab  = 100 * sum(group >= 3 ) / n(),
      mention_b   = 100 * sum(group >= 4 ) / n(),
      mention_tb  = 100 * sum(group == 5 ) / n()
    )

}
resultats( "Romain" )
resultats( "Diane" )
resultats( "Colin" )
resultats( "Vincent" )
