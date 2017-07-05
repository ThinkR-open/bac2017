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

data2 <- map2_df( data$href, data$academie, resultat_academie)

res <- llply( data2$url, resultat, .progress = "text" )
data2$data <- res

bac2017 <- unnest( data2 ) %>%
  mutate(
    ville = str_replace_all( str_to_title( str_replace( Établissement, " CEDEX.*$", "") ), " ", "-" ),
    famille = str_to_title(str_replace( Nom, "[^[:space:]]+$", "" ) ),
    prenoms = str_to_title(str_replace( Nom, "^[^[:space:]]+[[:space:]]", "" )),
    prenom  = str_replace( prenoms, "^[^[:space:]]+[[:space:]]", "" )
  ) %>%
  rename( Resultat = `Résultat`, Etablissement = `Établissement` )

cities <- unique(bac2017$ville)
x <- geocode(cities, limit = 1, quiet = TRUE)

bac2017 <- left_join( bac2017, x, by = c("ville" = "location") ) %>%
  select(-housenumber, -street, -city, -country, -osm_key, -msg)

use_data( bac2017, overwrite = TRUE )



