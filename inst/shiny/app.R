library(shiny)
library(leaflet)
library(bac2017)
library(prenoms)
library(sf)

ui <- fluidPage(theme = "bac.css",

  div( class = "fullpage",
    leafletOutput("carte", width = "100%", height = "100%")
  )

)

server <- function(input, output) {

  admis <- reactive({
    bac2017 %>%
      group_by( numero_departement ) %>%
      summarise(
        nom_departement = first(nom_departement),
        p_admis = sum(Resultat != "PASSE SECOND GROUPE" ) / n()
      )
  })

  output$carte <- renderLeaflet({
    data <- left_join( departements, admis(), by = c("code_insee" = "numero_departement") ) %>%
      mutate(
        col = gray( 1 - ( p_admis - min(p_admis) ) / (max(p_admis) - min(p_admis))),
        label = sprintf( "%s (%4.2f)", nom_departement, round(p_admis * 100, 2) )
      )

    leaflet(data) %>%
      addTiles( urlTemplate = 'http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png' ) %>%
      setView(lng = 5, lat= 47, zoom=7) %>%
      addPolygons( color = "black", weight = .5, fillColor = ~col, fill = TRUE, fillOpacity = .7, label = ~label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

