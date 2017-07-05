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

data3 <- unnest( data2 ) %>%
  mutate(
    ville = str_replace_all( str_to_title( str_replace( Établissement, " CEDEX.*$", "") ), " ", "-" ),
    famille = str_to_title(str_replace( Nom, "[^[:space:]]+$", "" ) ),
    prenoms = str_to_title(str_replace( Nom, "^[^[:space:]]+[[:space:]]", "" )),
    prenom  = str_to_title(str_replace( prenoms, "[[:space:]][^[:space:]]+$", "" ) )
  ) %>%
  rename( Resultat = `Résultat`, Etablissement = `Établissement` ) %>%
  select( -url, -Nom) %>%
  mutate(
    ville = clean_ville(ville)
  )

# cas particuliers
data3$ville[ data3$ville == "Terrasson La Villedi"] <- "Terrasson Lavilledieu"
data3$ville[ data3$ville == "Bordeaux Fondaudege"] <- "Bordeaux"
data3$ville[ data3$ville == "Saint Vincent De Tyross"] <- "Saint Vincent De Tyrosse"
data3$ville[ data3$ville == "Mauleon Soule"] <- "Mauleon"
data3$ville[ data3$ville == "Cherbourg En Cotentin"] <- "Cherbourg Octeville"
data3$ville[ data3$ville == "Mortain Bocage"] <- "Mortain"
data3$ville[ data3$ville == "Saint Pierre S Dives"] <- "Saint Pierre Sur Dives"
data3$ville[ data3$ville == "Cergy Le Haut"] <- "Cergy"
data3$ville[ data3$ville == "Cergy Pontoise"] <- "Cergy"
data3$ville[ data3$ville == "Les Ulis Ville"] <- "Les Ulis"
data3$ville[ data3$ville == "Limours"] <- "Limours En Hurepoix"
data3$ville[ data3$ville == "La Queue Lez Yvelines"] <- "La Queue Les Yvelines"
data3$ville[ data3$ville == "Cesson Vert Saint Denis"] <- "Vert Saint Denis"
data3$ville[ data3$ville == "Pecq Sur Seine"] <- "Le Pecq"
data3$ville[ data3$ville == "Chalon Saone"] <- "Chalon Sur Saone"
data3$ville[ data3$ville == "Chatillon Seine"] <- "Chatillon Sur Seine"
data3$ville[ data3$ville == "Cosne Sur Loire"] <- "Cosne Cours Sur Loire"
data3$ville[ data3$ville == "Saint Julien En Genevois Cede"] <- "Saint Julien En Genevois"
data3$ville[ data3$ville == "La Mure D Isere"] <- "La Mure"
data3$ville[ data3$ville == "Thonon"] <- "Thonon Les Bains"
data3$ville[ data3$ville == "Le Pont De Beauvoisin"] <- "Pont De Beauvoisin"
data3$ville[ data3$ville == "Bourgoin Jallieu"] <- "Bourgoin"
data3$ville[ data3$ville == "Villefranche Saone"] <- "Villefranche Sur Saone"
data3$ville[ data3$ville == "Charbonnieres Les Ba"] <- "Charbonnieres Les Bains"
data3$ville[ data3$ville == "Bellegarde Valserine"] <- "Bellegarde Sur Valserine"
data3$ville[ data3$ville == "Villeneuve Avignon" ] <- "Villeneuve Les Avignon"
data3$ville[ data3$ville == "Saint Die Des Vosges" ] <- "Saint Die"
data3$ville[ data3$ville == "La Baule" ] <- "Baule"
data3$ville[ data3$ville == "Chemille En Anjou" ] <- "Chemille"
data3$ville[ data3$ville == "Machecoul Saint Meme" ] <- "Machecoul"
data3$ville[ data3$ville == "Saint Sebastien Loire" ] <- "Saint Sebastien Sur Loire"
data3$ville[ data3$ville == "Saintes Air" ] <- "Saintes"
data3$ville[ data3$ville == "Bagneres De Big" ] <- "Bagneres De Bigorre"


communes <- as_tibble(read.csv2( "data-raw/eucircos_regions_departements_circonscriptions_communes_gps.csv", stringsAsFactors = FALSE)) %>%
  rename(
    nom_departement = `nom_département`,
    numero_departement = `numéro_département`,
    ville = nom_commune
  ) %>%
  select( nom_departement, numero_departement, ville, latitude, longitude ) %>%
  mutate( ville = clean_ville(ville) )
communes$ville[ communes$ville == "Domfront" & communes$numero_departement == "60" ] <- "Domfront En Poiraie"
communes$ville[ communes$ville == "Vire" & communes$numero_departement == "14" ] <- "Vire Normandie"
communes$ville[ communes$ville == "Carentan" & communes$numero_departement == "50" ] <- "Carentan Les Marais"
communes$ville[ communes$ville == "Conde Sur Noireau" & communes$numero_departement == "14" ] <- "Conde En Normandie"
communes$ville[ communes$ville == "Torcy" & communes$numero_departement == "77" ] <- "Torcy Marne La Vallee"
communes$ville[ communes$ville == "Teil (Le)" ] <- "Le Teil"
communes$ville[ communes$ville == "Cheylard (Le)" ] <- "Le Cheylard"

setdiff( unique(data3$ville), communes$ville )
filter( communes, grepl("Terrasson", ville))

communes <- distinct( communes, ville, .keep_all = TRUE )

bac2017 <- left_join( data3, communes, by = "ville") %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

use_data( bac2017, overwrite = TRUE )



