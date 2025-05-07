library(shiny)
library(DT)
library(readxl)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)

employes <- read_excel("../Data/employees.xlsx")

employes_ui <- function(id){
  ns <- NS(id)

  fluidPage(
  tags$head(tags$style(HTML("body {font-family: 'Segoe UI', sans-serif;} 
    .box-style {background-color: #ffffff; border: 1px solid #dee2e6; border-radius: 10px; padding: 15px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);}
    .title-box {font-weight: bold; font-size: 16px; margin-bottom: 10px;}
    .stat-box {background-color: #eaf7ff; border-left: 5px solid #007bff; padding: 10px; border-radius: 8px; margin-bottom: 10px;}"))),
  
  div(
    style = "background: linear-gradient(90deg, #007bff, #00c6ff); padding: 25px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);",
    h1(
      tags$i(class = "fas fa-users-cog", style = "margin-right: 15px;"),
      "TCHOP ET YAMO – Tableau de bord RH",
      style = "color: white; font-size: 32px; font-weight: 600; text-align: center; margin: 0;"
    )
  ),
  
  
  
  fluidRow(
    column(3, div(class = "stat-box", h4("Nombre total d'employés"), h3(textOutput(ns("nb_total"))))),
    column(3, div(class = "stat-box", h4("Nombre de femmes"), h3(textOutput(ns("nb_femmes"))))),
    column(3, div(class = "stat-box", h4("Nombre d'hommes"), h3(textOutput(ns("nb_hommes"))))),
    column(3, div(class = "stat-box", h4("Nombre de nationalités"), h3(textOutput(ns("nb_nationalites")))))
  ),
  
  fluidRow(
    column(6, div(class = "box-style", plotlyOutput(ns("bar_sexe")))),
    column(6, div(class = "box-style", plotlyOutput(ns("bar_nationalite"))))
  ),
  fluidRow(
    column(6, div(class = "box-style", plotlyOutput(ns("bar_masse_sexe")))),
    column(6, div(class = "box-style", plotlyOutput(ns("bar_masse_region"))))
  ),
  fluidRow(
    column(6, div(class = "box-style", plotlyOutput(ns("bar_masse_nationalite"))))
  ),
  
  fluidRow(
    column(3, div(style = "background-color: #fdeff4; border-left: 6px solid #e83e8c; border-radius: 12px; padding: 15px; box-shadow: 0 2px 5px rgba(0,0,0,0.05);",
                  div(style = "font-weight: bold; font-size: 16px; color: #e83e8c; margin-bottom: 10px;", "👩‍💼 Meilleur Employé"),
                  uiOutput(ns("best_employee")))),
    
    column(3, div(style = "background-color: #eaf5ff; border-left: 6px solid #007bff; border-radius: 12px; padding: 15px; box-shadow: 0 2px 5px rgba(0,0,0,0.05);",
                  div(style = "font-weight: bold; font-size: 16px; color: #007bff; margin-bottom: 10px;", "🛵 Meilleur Livreur"),
                  uiOutput(ns("best_livreur")))),
    
    column(6, div(style = "background-color: skyblue; border-radius: 12px; padding: 20px; box-shadow: 0 2px 5px rgba(0,0,0,0.05);",
                  div(style = "font-weight: bold; font-size: 16px; color: #333; margin-bottom: 10px;", "🎯 Filtres RH"),
                  fluidRow(
                    column(4, selectInput(ns("sex"), "Sexe :", choices = c("Tous", unique(employes$sexe)), selected = "Tous")),
                    column(4, selectInput(ns("région"), "Région :", choices = c("Tous", unique(employes$région)), selected = "Tous")),
                    column(4, selectInput(ns("poste"), "Poste :", choices = c("Tous", unique(employes$poste)), selected = "Tous"))
                  )))),
  
  
  fluidRow(
    column(12, div(class = "box-style", dataTableOutput(ns("table_partial"))))
  )
)
}

employes_server <- function(id){
  

  moduleServer(id, function(input, output, session) {
  
  filteredData <- reactive({
    df <- employes
    if (!is.null(input$sex) && input$sex != "Tous") df <- df[df$sexe == input$sex, ]
    if (!is.null(input$région) && input$région != "Tous") df <- df[df$région == input$région, ]
    if (!is.null(input$poste) && input$poste != "Tous") df <- df[df$poste == input$poste, ]
    return(df)
  })
  
  selectedEmployee <- reactiveVal(NULL)
  
  observeEvent(input$table_partial_rows_selected, {
    req(input$table_partial_rows_selected)
    df <- filteredData()
    if (nrow(df) == 0 || input$table_partial_rows_selected > nrow(df)) return()
    selected <- df[input$table_partial_rows_selected, ]
    selectedEmployee(selected)
    showModal(modalDialog(
      title = paste("Fiche de", selected$prenom, selected$nom),
      fluidRow(
        column(4, tags$img(src = selected$photo, height = "150px", style = "border-radius:10px;")),
        column(8,
               h4(paste(selected$prenom, selected$nom)),
               p(strong("Poste : "), selected$poste),
               p(strong("Sexe : "), selected$sexe),
               p(strong("Âge : "), selected$age),
               p(strong("Nationalité : "), selected$nationalité),
               p(strong("Région : "), selected$région),
               p(strong("Salaire : "), paste0(selected$salaire, " FCFA")),
               p(strong("Avis moyen : "), selected$`avis moyen`)
        )
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  output$table_partial <- renderDataTable({
    req(filteredData())
    df <- filteredData()
    validate(
      need(nrow(df) > 0, "Aucun employé ne correspond aux filtres.")
    )
    df$photo <- sapply(1:nrow(df), function(i) {
      paste0("<img src='", df$photo[i], "' height='50' style='border-radius:50%;'>")
    })
    datatable(df, escape = FALSE, selection = 'single', options = list(pageLength = 5))
  })
  
  output$nb_total <- renderText({ nrow(employes) })
  output$nb_femmes <- renderText({ sum(employes$sexe == "F") })
  output$nb_hommes <- renderText({ sum(employes$sexe == "M") })
  output$nb_nationalites <- renderText({ length(unique(employes$nationalité)) })
  
  output$bar_sexe <- renderPlotly({
    df <- employes %>% count(sexe) %>% arrange(n)
    plot_ly(df, labels = ~sexe, values = ~n, type = 'pie', textinfo = 'label+percent') %>%
      layout(title = "Répartition par sexe")
  })
  
  output$bar_nationalite <- renderPlotly({
    df <- employes %>% count(nationalité) %>% arrange(n)
    plot_ly(df, x = ~reorder(nationalité, n), y = ~n, type = 'bar') %>%
      layout(title = "Répartition par nationalité", xaxis = list(title = "Nationalité"), yaxis = list(title = "Nombre"))
  })
  
  output$bar_masse_sexe <- renderPlotly({
    df <- employes %>% group_by(sexe) %>% summarise(masse = sum(salaire)) %>% arrange(masse)
    plot_ly(df, x = ~sexe, y = ~masse, type = 'bar', color = ~sexe, colors = c("F" = "#e83e8c", "M" = "#007bff")) %>%
      layout(title = "Masse salariale par sexe", xaxis = list(title = "Sexe"), yaxis = list(title = "Masse salariale"))
  })
  
  output$bar_masse_region <- renderPlotly({
    df <- employes %>% group_by(région) %>% summarise(masse = sum(salaire)) %>% arrange(masse)
    plot_ly(df, x = ~reorder(région, masse), y = ~masse, type = 'bar') %>%
      layout(title = "Masse salariale par région", xaxis = list(title = "Région"), yaxis = list(title = "Masse salariale"))
  })
  
  output$bar_masse_nationalite <- renderPlotly({
    df <- employes %>% group_by(nationalité) %>% summarise(masse = sum(salaire)) %>% arrange(masse)
    plot_ly(df, x = ~reorder(nationalité, masse), y = ~masse, type = 'bar') %>%
      layout(title = "Masse salariale par nationalité", xaxis = list(title = "Nationalité"), yaxis = list(title = "Masse salariale"))
  })
  
  output$best_employee <- renderUI({
    best <- employes %>% filter(poste != "Livreur") %>% arrange(desc(`avis moyen`)) %>% slice(1)
    HTML(paste0(
      "<img src='", best$photo, "' height='100' style='border-radius:50%;cursor:pointer;'><br>",
      "<b>", best$prenom, " ", best$nom, "</b><br>",
      best$poste, "<br>Avis: ", best$`avis moyen`
    ))
  })
  
  output$best_livreur <- renderUI({
    best <- employes %>% filter(grepl("[Ll]ivreur", poste)) %>% arrange(desc(`avis moyen`)) %>% slice(1)
    HTML(paste0(
      "<img src='", best$photo, "' height='100' style='border-radius:50%;cursor:pointer;'><br>",
      "<b>", best$prenom, " ", best$nom, "</b><br>",
      best$poste, "<br>Avis: ", best$`avis moyen`
    ))
  })
  }
)
}

#shinyApp(ui = ui, server = server)
