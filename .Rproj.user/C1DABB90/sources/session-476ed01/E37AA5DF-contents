library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)
library(plotly)
library(lubridate)
library(readxl)

# Chargement des données
commande <- read.csv("Data/commandes.csv")
livraison <- read.csv("Data/livraisons.csv")
plat <- read_excel("Data/plat.xlsx")

regions_livraison <- st_read("Data/SEN_adm1/SEN_adm1.shp")

regions_livraison <- regions_livraison[, c("ID_region", "NAME_1", "geometry")]
colnames(regions_livraison)[1:2] <- c("ID_region", "region")

commande$date_commande <- as.POSIXct(commande$date_commande, format = "%Y-%m-%d %H:%M:%S")
livraison$date_livraison <- as.POSIXct(livraison$date_livraison, format = "%Y-%m-%d %H:%M:%S")

# Fonction pour format K/M
format_abbr <- function(x) {
  if (is.na(x)) return("NA")
  if (x >= 1e6) {
    return(paste0(round(x / 1e6, 1), "M"))
  } else if (x >= 1e3) {
    return(paste0(round(x / 1e3, 1), "K"))
  } else {
    return(as.character(round(x, 2)))
  }
}

# UI
livraison_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    tags$style(HTML(".value-box .small-box p { font-weight: bold; font-size: 40px; color: #000; }")),
  
    fluidRow(
      column(3, dateRangeInput(ns("date_range"), "Période", start = Sys.Date() - 30, end = Sys.Date())),
      column(3, selectInput(ns("plats"), "Plats", choices = unique(plat$nom_plat), multiple = TRUE)),
      column(3, selectInput(ns("categories"), "Catégories", choices = unique(plat$type), multiple = TRUE)),
      column(3, selectInput(ns("regions"), "Régions", choices = unique(regions_livraison$region), multiple = TRUE))
    ),
   
      
              fluidRow(
                valueBoxOutput(ns("revenus_commandes"), width = 3),
                valueBoxOutput(ns("revenus_livraisons"), width = 3),
                valueBoxOutput(ns("benefice_net"), width = 3),
                valueBoxOutput(ns("temps_moyen_livraison"), width = 3)
              ),
              fluidRow(
                valueBoxOutput(ns("total_commandes"), width = 3),
                valueBoxOutput(ns("commandes_livrees"), width = 3),
                valueBoxOutput(ns("commandes_attente"), width = 3),
                valueBoxOutput(ns("commandes_annulees"), width = 3)
              ),
              fluidRow(
                box(title = "Carte des ventes par région", width = 6, leafletOutput(ns("map"), height = 400)),
                box(title = "Évolution des ventes dans le temps", width = 6, plotlyOutput(ns("evol_commandes")))
              ),
              fluidRow(
                box(title = "Top plats commandés", width = 6, plotlyOutput(ns("top_plats"))),
                box(title = "Top catégories commandées", width = 6, plotlyOutput(ns("top_categories")))
              )
      
    
  )
}

# Server
livraison_server <- function(id){
  

  moduleServer(id, function(input, output, session) {
  
  observe({
    if (is.null(input$categories) || length(input$categories) == 0) {
      plats_filtrés <- unique(plat$nom_plat)
    } else {
      plats_filtrés <- unique(plat %>% filter(type %in% input$categories) %>% pull(nom_plat))
    }
    updateSelectInput(session, "plats", choices = plats_filtrés, selected = intersect(input$plats, plats_filtrés))
  })
  
  commande_filtered <- reactive({
    data_livraison <- commande %>%
      left_join(plat, by = "id_plat") %>%
      filter(date_commande >= input$date_range[1],
             date_commande <= input$date_range[2])
    
    data_livraison <- data_livraison %>% left_join(regions_livraison %>% st_drop_geometry(), by = "ID_region")
    
    if (!is.null(input$plats) && length(input$plats) > 0) {
      data_livraison <- data_livraison %>% filter(nom_plat %in% input$plats)
    }
    if (!is.null(input$categories) && length(input$categories) > 0) {
      data_livraison <- data_livraison %>% filter(type %in% input$categories)
    }
    if (!is.null(input$regions) && length(input$regions) > 0) {
      data_livraison <- data_livraison %>% filter(region %in% input$regions)
    }
    data_livraison
  })
  
  output$total_commandes <- renderValueBox({
    valueBox(nrow(commande_filtered()), "Total Commandes", icon = icon("shopping-cart"), color = "blue")
  })
  
  output$commandes_livrees <- renderValueBox({
    valueBox(sum(commande_filtered()$status == "Livrée"), "Commandes livrées", icon = icon("check"), color = "green")
  })
  
  output$commandes_attente <- renderValueBox({
    valueBox(sum(commande_filtered()$status == "En attente"), "Commandes en attente", icon = icon("clock"), color = "orange")
  })
  
  output$commandes_annulees <- renderValueBox({
    valueBox(sum(commande_filtered()$status == "Annulée"), "Commandes annulées", icon = icon("times"), color = "red")
  })
  
  output$revenus_commandes <- renderValueBox({
    total <- sum(commande_filtered()$qte * commande_filtered()$prix, na.rm = TRUE)
    valueBox(paste0(format_abbr(total), " FCFA"), "Revenus des commandes", icon = icon("euro-sign"), color = "orange")
  })
  
  output$revenus_livraisons <- renderValueBox({
    total <- livraison %>%
      filter(id_commande %in% commande_filtered()$id_commande) %>%
      pull(montant_livraison) %>%
      sum(na.rm = TRUE)
    valueBox(paste0(format_abbr(total), " FCFA"), "Revenus des livraisons", icon = icon("euro-sign"), color = "teal")
  })
  
  output$temps_moyen_livraison <- renderValueBox({
    merged <- livraison %>%
      filter(id_commande %in% commande_filtered()$id_commande) %>%
      left_join(commande_filtered(), by = "id_commande") %>%
      mutate(duree = as.numeric(difftime(date_livraison, date_commande, units = "mins")))
    moy <- mean(merged$duree, na.rm = TRUE)
    valueBox(paste0(round(moy, 1), " min"), "Temps moyen livraison", icon = icon("clock"), color = "olive")
  })
  
  output$benefice_net <- renderValueBox({
    rev_commande <- sum(commande_filtered()$qte * commande_filtered()$prix, na.rm = TRUE)
    rev_livraison <- livraison %>%
      filter(id_commande %in% commande_filtered()$id_commande) %>%
      pull(montant_livraison) %>%
      sum(na.rm = TRUE)
    benef <- rev_commande + rev_livraison
    valueBox(paste0(format_abbr(benef), " FCFA"), "Bénéfice net", icon = icon("chart-line"), color = "fuchsia")
  })
  
  output$top_plats <- renderPlotly({
    top <- commande_filtered() %>%
      count(nom_plat, sort = TRUE) %>%
      top_n(5, n)
    plot_ly(top, x = ~n, y = ~reorder(nom_plat, n), type = 'bar', orientation = 'h',
            marker = list(color = ~n, colorscale = 'Viridis')) %>%
      layout(title = "", xaxis = list(title = ""), yaxis = list(title = "Plat"))
  })
  
  output$top_categories <- renderPlotly({
    top <- commande_filtered() %>%
      count(type, sort = TRUE) %>%
      top_n(5, n)
    plot_ly(top, x = ~n, y = ~reorder(type, n), type = 'bar', orientation = 'h',
            marker = list(color = ~n, colorscale = 'Cividis')) %>%
      layout(title = "", xaxis = list(title = ""), yaxis = list(title = "Catégorie"))
  })
  
  output$evol_commandes <- renderPlotly({
    evo <- commande_filtered() %>%
      mutate(semaine = floor_date(date_commande, "week")) %>%
      count(semaine)
    plot_ly(evo, x = ~semaine, y = ~n, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Évolution des commandes", xaxis = list(title = "Date"), yaxis = list(title = "Commandes"))
  })
  
  output$map <- renderLeaflet({
    count_data <- commande_filtered() %>%
      group_by(ID_region) %>%
      summarise(nb_commandes = n(), .groups = "drop")
    
    data_map <- left_join(regions_livraison, count_data, by = "ID_region")
    
    pal <- colorNumeric("YlOrRd", domain = data_map$nb_commandes)
    
    leaflet(data_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(nb_commandes),
                  color = "white", weight = 1, fillOpacity = 0.7,
                  popup = ~paste(region, ":", nb_commandes, "commandes")) %>%
      addLegend(pal = pal, values = ~nb_commandes, title = "Commandes")
  })
  }
)
}

#shinyApp(ui, server)