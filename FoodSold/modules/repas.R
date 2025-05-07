library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(leaflet)
library(plotly)
library(shinycssloaders)

# Charger les données depuis Excel
plats_data <- read_excel("../Data/plats.xlsx")
plats_data$note <- as.numeric(plats_data$note)

# Fonction pour afficher les étoiles
generateStars <- function(note) {
  if (is.null(note) || is.na(note) || note < 1 || note > 5) {
    return(HTML('<span style="color:gray;">Pas d\'avis</span>'))
  }
  pleines <- strrep("★", floor(note))
  vides <- strrep("☆", 5 - floor(note))
  HTML(paste0(pleines, vides))
}

# UI
repas_ui <- function(id){
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel("Informations",
             h3("Liste des plats"),
             br(),
             fluidRow(
               column(4,
                      selectInput(ns("type_plat"), "Type de plat :",
                                  choices = c("Tous", sort(unique(plats_data$type))),
                                  selected = "Tous")
               ),
               column(4,
                      textInput(ns("recherche_plat"), "Rechercher un plat :", "")
               ),
               column(4,
                      selectInput(ns("tri"), "Trier par :",
                                  choices = c("Aucun", "Prix croissant", "Prix décroissant", "Note décroissante", "Note croissante"))
               )
             ),
             br(),
             uiOutput(ns("plats_ui"))
    ),
    tabPanel("Suivi",
             h3("Suivi des plats"),
             div(
               style = "background-image: url('font.png');
                                    background-size: cover;
                                    background-position: center;
                                    padding: 20px; border-radius: 10px;",
               fluidRow(
                 box(title = "Commandes par plat", status = "primary", solidHeader = TRUE,
                     width = 6, plotlyOutput(ns("plot_commandes"))),
                 
                 box(title = "Répartition des commandes par type", status = "success", solidHeader = TRUE,
                     width = 6, plotlyOutput(ns("plot_pie_type")))
               )
             )
    )
  ) # fin de tabset
}

# Server
repas_server <- function(id){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  output$plats_ui <- renderUI({
    plats_filtrés <- plats_data
    
    if (input$type_plat != "Tous") {
    
      plats_filtrés <- plats_filtrés[plats_filtrés$type == input$type_plat, ]
    }
    
    if (input$recherche_plat != "") {
      plats_filtrés <- plats_filtrés[grepl(input$recherche_plat, plats_filtrés$nom_plat, ignore.case = TRUE), ]
    }
    
    if ("note" %in% names(plats_filtrés)) {
      if (input$tri == "Prix croissant") {
        plats_filtrés <- plats_filtrés[order(plats_filtrés$Prix), ]
      } else if (input$tri == "Prix décroissant") {
        plats_filtrés <- plats_filtrés[order(-plats_filtrés$Prix), ]
      } else if (input$tri == "Note décroissante") {
        plats_filtrés <- plats_filtrés[order(-plats_filtrés$note), ]
      } else if (input$tri == "Note croissante") {
        plats_filtrés <- plats_filtrés[order(plats_filtrés$note), ]
      }
    }
    
    if (nrow(plats_filtrés) == 0) {
      return(tags$div(style = "text-align:center; font-size:18px; color:red;", "Aucun plat trouvé."))
    }
    
    rows <- list()
    for (i in seq(1, nrow(plats_filtrés), by = 4)) {
      row <- fluidRow(
        lapply(0:3, function(j) {
          idx <- i + j
          if (idx <= nrow(plats_filtrés)) {
            column(3,
                   actionButton(
                     inputId = ns(paste0("plat_", plats_filtrés$id_plat[idx])),
                     label = div(class = "plat-container",
                                 img(src = plats_filtrés$image[idx], class = "plat-image"),
                                 h4(plats_filtrés$nom_plat[idx]),
                                 p(paste("Prix :", plats_filtrés$Prix[idx], "F")),
                                 p(paste("Commandes :", plats_filtrés$nombre_commandes[idx])),
                                 p(generateStars(plats_filtrés$note[idx]))
                     ),
                     style = "width:100%; padding:0; border:none; background:none;"
                   )
            )
          } else {
            NULL
          }
        })
      )
      rows <- append(rows, list(row))
    }
    
    do.call(tagList, rows)
  })
    
    lapply(plats_data$id_plat, function(id_val) {
      observeEvent(input[[paste0("plat_", id_val)]], {
        print(paste("Click d"tect" pour le plat d'id : ", id_val))  # TEST
        
        plat <- plats_data[plats_data$id_plat == id_val, ]
        
        showModal(modalDialog(
          title = plat$nom_plat,
          size = "l",
          fluidRow(
            column(6,
                   tags$img(src = plat$image, style = "width:250px; height:auto; border-radius:10px;")
            ),
            column(6,
                   tags$h4(plat$type,":", plat$nom_plat),
                   tags$p(tags$b("Prix :"), paste(plat$Prix, "F")),
                   tags$p(tags$b("Commandes :"), plat$nombre_commandes),
                   tags$p(tags$b("Note :"), generateStars(plat$note)),
                   tags$p(tags$b("Description:"), plat$description)
            )
          ),
          easyClose = TRUE,
          footer = modalButton("Fermer")
        )) # Fin du modal
        
      })
    })
  
  
  output$plot_commandes <- renderPlotly({
    p <- ggplot(plats_data, aes(x = reorder(nom_plat, nombre_commandes), y = nombre_commandes)) +
      geom_bar(stat = "identity", fill = "#0073C2FF") +
      coord_flip() +
      labs(title = "Nombre de commandes par plat", x = "Plat", y = "Commandes") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$plot_pie_type <- renderPlotly({
    commandes_par_type <- plats_data %>%
      group_by(type) %>%
      summarise(total_commandes = sum(nombre_commandes, na.rm = TRUE)) %>%
      filter(!is.na(type))
    
    plot_ly(
      commandes_par_type,
      labels = ~type,
      values = ~total_commandes,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial'
    ) %>%
      layout(title = "Répartition des commandes par type de plat")
  })
  
  }  
  )
}

#shinyApp(ui, server)
