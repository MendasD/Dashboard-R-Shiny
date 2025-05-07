library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(readxl)
library(scales)
library(lubridate)

# UI
salaire_ui <- function(id){
  ns <- NS(id)

#   dashboardPage(
#   skin = "blue",
#   dashboardHeader(title = "Masse Salariale - Tchop et Yamo"),
#   
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Masse Salariale", tabName = "main", icon = icon("money-bill-wave"))
#     )
#   ),
#   
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "main",
#               
#       )
#     )
#   )
# )
  
  fluidPage(
    div(
      style = "background: linear-gradient(90deg, #007bff, #00c6ff); padding: 25px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);",
      h1(
        tags$i(class = "fas fa-credit-card", style = "margin-right: 15px;"),
        "TCHOP ET YAMO – Gestion des salaires",
        style = "color: white; font-size: 32px; font-weight: 600; text-align: center; margin: 0;"
      )
    ),
    
    box(width = 12,
        title = "Filtres",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        fluidRow(
          column(6,
                 selectInput(
                   ns("region"), 
                   label = "Région:",
                   choices = NULL,
                   selected = "Toutes",
                   width = "100%"
                 )
          ),
          column(6,
                 sliderInput(
                   ns("date_arrivee"),
                   label = "Année d'arrivée:",
                   min = 2010,
                   max = 2025,
                   value = c(2010, 2025),
                   sep = "",
                   width = "100%"
                 )
          )
        )
    ),
    
    fluidRow(
      valueBoxOutput(ns("total_salaire"), width = 4),
      valueBoxOutput(ns("moyenne_salaire"), width = 4),
      valueBoxOutput(ns("effectif"), width = 4)
    ),
    
    fluidRow(
      box(plotlyOutput(ns("camembert_sexe")), width = 6,
          title = "Répartition par sexe"),
      box(plotlyOutput(ns("camembert_nationalite")), width = 6,
          title = "Répartition par nationalité")
    ),
    
    fluidRow(
      box(plotlyOutput(ns("bar_poste")), width = 6,
          title = "Masse salariale par poste"),
      box(plotlyOutput(ns("bar_contrat")), width = 6,
          title = "Masse salariale par type de contrat")
    )
    # Fin du main
  )
}

# Server
salaire_server <- function(id){
  

  moduleServer(id, function(input, output, session) {
  
  employes <- reactive({
    tryCatch({
      df <- read_excel("../Data/emplois.xlsx") %>%
        mutate(annee_arrivee = as.integer(date_arrivee))
      
      updateSelectInput(session, "region",
                        choices = c("Toutes", unique(df$region)))
      
      updateSliderInput(session, "date_arrivee",
                        min = min(df$date_arrivee),
                        max = max(df$date_arrivee),
                        value = c(min(df$date_arrivee), max(df$date_arrivee)))
      
      return(df)
    }, error = function(e) {
      showNotification(paste("Erreur de lecture du fichier:", e$message), type = "error")
      return(NULL)
    })
  })
  
  data_filtered <- reactive({
    req(employes())
    df <- employes()
    
    if (!is.null(input$region) && input$region != "Toutes") {
      df <- df %>% filter(region == input$region)
    }
    
    df %>% filter(
      date_arrivee >= input$date_arrivee[1],
      date_arrivee <= input$date_arrivee[2]
    )
  })
  
  output$total_salaire <- renderValueBox({
    req(data_filtered())
    total <- sum(data_filtered()$salaire)
    valueBox(
      paste(format(total, big.mark = " "), "FCFA"), 
      "Masse salariale totale",
      icon = icon("money-bill-wave"),
      color = "green"
    )
  })
  
  output$moyenne_salaire <- renderValueBox({
    req(data_filtered())
    moyenne <- mean(data_filtered()$salaire)
    valueBox(
      paste(format(round(moyenne), big.mark = " "), "FCFA"), 
      "Salaire moyen",
      icon = icon("calculator"),
      color = "blue"
    )
  })
  
  output$effectif <- renderValueBox({
    req(data_filtered())
    effectif <- nrow(data_filtered())
    valueBox(
      effectif, 
      "Effectif total",
      icon = icon("users"),
      color = "red"
    )
  })
  
  output$camembert_sexe <- renderPlotly({
    req(data_filtered())
    data_filtered() %>%
      group_by(sexe) %>%
      summarise(masse = sum(salaire)) %>%
      plot_ly(labels = ~sexe, values = ~masse) %>%
      add_pie(hole = 0.5) %>%
      layout(showlegend = TRUE)
  })
  
  output$camembert_nationalite <- renderPlotly({
    req(data_filtered())
    data_filtered() %>%
      group_by(nationalite) %>%
      summarise(masse = sum(salaire)) %>%
      plot_ly(labels = ~nationalite, values = ~masse) %>%
      add_pie(hole = 0.5) %>%
      layout(showlegend = TRUE)
  })
  
  output$bar_poste <- renderPlotly({
    req(data_filtered())
    ggplotly(
      data_filtered() %>%
        group_by(poste) %>%
        summarise(masse = sum(salaire)) %>%
        ggplot(aes(x = reorder(poste, masse), y = masse, fill = poste)) +
        geom_col() +
        geom_text(aes(label = paste(format(masse, big.mark = " "), "FCFA")), 
                  hjust = -0.1, size = 3) +
        labs(x = "", y = "") +
        coord_flip() +
        theme_minimal()
    )
  })
  
  output$bar_contrat <- renderPlotly({
    req(data_filtered())
    ggplotly(
      data_filtered() %>%
        group_by(type_contrat) %>%
        summarise(masse = sum(salaire)) %>%
        ggplot(aes(x = reorder(type_contrat, masse), y = masse, fill = type_contrat)) +
        geom_col() +
        geom_text(aes(label = paste(format(masse, big.mark = " "), "FCFA")), 
                  hjust = -0.1, size = 3) +
        labs(x = "", y = "") +
        coord_flip() +
        theme_minimal()
    )
  })
  }
  )
}

#shinyApp(ui, server)
