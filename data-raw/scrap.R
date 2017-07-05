library(rvest)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(photon)

llply <- plyr::llply

links <- read_html("http://www.letudiant.fr/resultat/bac.html") %>%
  html_nodes("li.c-list--dotted__item a")

data <- tibble(
    href = paste0( "http://www.letudiant.fr", html_attr(links, "href") ),
    title = html_attr(links, "title")
  ) %>%
  filter( grepl( "^Tous", title ) ) %>%
  mutate( academie = str_replace(title, "^.*académie ", "") ) %>%
  select(-title)

resultat_academie <- function(link, academie ){
  links <- read_html(link) %>%
    html_nodes(".c-list--dotted__item a") %>%
    html_attr("href") %>%
    str_subset("^/resultat")

  option <- str_replace( links, "^.*/(.*).html", "\\1" )

  base <- paste0( "http://www.letudiant.fr", str_replace( links, ".html$", "" ) )

  tibble(
    url = paste0( rep( base, each = 26 ), "/", letters, ".html" ),
    academie = academie,
    option = rep( option, each = 26)
  )
}

resultat <- function(url){
  read_html( url ) %>%
    html_node(".c-box--wire--table") %>%
    html_table(fill = TRUE) %>%
    filter( complete.cases(.) )
}

data2 <- map2_df( data$href, data$academie, resultat_academie) %>%
  mutate( data = llply( url, resultat, .progress = "text" ) )

clean_ville <- function(ville){
  ville %>%
    str_replace_all( "[/]S[/]", " Sur " ) %>%
    str_replace_all( " S ", " Sur " ) %>%
    str_replace_all( "[-/]+", " " ) %>%
    str_replace_all("[éèêë]", "e") %>%
    str_replace_all("[ç]", "c") %>%
    str_replace_all("[àâ]", "a") %>%
    str_replace_all("[ô]", "o") %>%
    str_replace_all("[û]", "u") %>%
    str_replace_all("[î]", "i") %>%
    str_replace_all("'", " ") %>%
    str_to_title() %>%
    str_replace_all( "St ", "Saint ") %>%
    str_replace_all( "Ste ", "Sainte ") %>%
    str_replace_all( " *$", "" ) %>%
    str_replace_all( " S ", " Sur " )
}

data4 <- unnest( data2 ) %>%
  rename( Resultat = `Résultat`, Etablissement = `Établissement` ) %>%
  mutate(
    ville = str_replace_all( str_to_title( str_replace( Etablissement, " CEDEX.*$", "") ), " ", "-" ),
    Nom = str_to_title( Nom )
  ) %>%
  select( -url )

villes <- pull(data4, ville)

xx <- map_df( villes, ~ geocode(.) %>% filter( country == "France", osm_value %in% c("city", "town", "village") ) %>% head(1) )
xx <- select( xx, location, postcode, state, osm_value, lon, lat)

bac2017 <- left_join( data4, xx, by = c( ville = "location"))
bac2017 <- mutate( bac2017, postcode = map_chr(str_split(postcode, ";"), 1 ) )

use_data( bac2017, overwrite = TRUE )



