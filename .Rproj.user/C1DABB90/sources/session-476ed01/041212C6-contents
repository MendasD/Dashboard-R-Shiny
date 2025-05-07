# install.packages("shinydashboardPlus")
library(shiny)
library(shinydashboardPlus)
library(bs4Dash)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(rsconnect)
library(dplyr)
library(DT)
library(sparkline) # pour les mini-graphes
library(plotly)
library(RColorBrewer)
library(sf)         # Pour lire et manipuler les shapefiles
library(leaflet)    # Pour créer des cartes interactives
library(visNetwork) # pour l'organigramme
library(shinycssloaders)

source("modules/salaires.R")
source("modules/employes.R")
source("utils.R")
source("modules/repas.R")
source("modules/livraison.R")

#Chargement des donnees de livraison
commande_livraison <- read.csv("Data/commandes.csv")
livraison <- read.csv("Data/livraisons.csv")
plat_livraison <- read_excel("Data/plat.xlsx")

regions_livraison <- st_read("Data/SEN_adm1/SEN_adm1.shp")


# Liste exhaustive des régions et de quelques villes associées
regions <- c("Dakar", "Thiès", "Saint-Louis", "Ziguinchor", "Kaolack", "Fatick")
regions_villes <- list(
  "Dakar" = c("Dakar", "Guédiawaye", "Pikine", "Rufisque"),
  "Thiès" = c("Thiès", "Mbour", "Tivaouane", "Khombole"),
  "Saint-Louis" = c("Saint-Louis", "Dagana", "Podor", "Richard-Toll"),
  "Ziguinchor" = c("Ziguinchor", "Bignona", "Oussouye"),
  "Kaolack" = c("Kaolack", "Nioro", "Guinguinéo"),
  "Fatick" = c("Fatick", "Foundiougne", "Gossas"),
  "Kaffrine" = c("Kaffrine", "Birkilane", "Malem Hodar"),
  "Kolda" = c("Kolda", "Vélingara", "Médina Yoro Foulah"),
  "Tambacounda" = c("Tambacounda", "Bakel", "Koumpentoum"),
  "Matam" = c("Matam", "Kanel", "Ranérou"),
  "Sédhiou" = c("Sédhiou", "Bounkiling", "Goudomp"),
  "Louga" = c("Louga", "Kébémer", "Linguère"),
  "Diourbel" = c("Diourbel", "Bambey", "Mbacké"),
  "Kédougou" = c("Kédougou", "Salémata", "Saraya")
)

# Base des employés
df <- readxl::read_excel("Data/employes.xlsx")

# === Données d’exemple ===
data <- data.frame(
  id = 1:18,
  prenom = c("Mamadou", "Cheikh", "Fatou", "Aliou", "Issa", "Serge",
             "Victorine", "Adama", "Alioune", "Cheikh", "Khady", "Souleymane",
             "Mamadou", "Cheikh", "Clarisse", "Moussa", "Moussa", "Souleymane"),
  nom = c("Sarr", "Diouf", "Ndiaye", "Gueye", "Ndiaye", "Mbarga",
          "Mbarga", "Diagne", "Lô", "Lô", "Ndiaye", "Diagne",
          "Fall", "Coly", "Ewane", "Thiam", "Sy", "Coly"),
  poste = c(
    "Directeur Général",               # id = 1
    "Responsable régional",           # Dakar
    "Chef de cuisine", "Responsable livraison", "Responsable service client", "Responsable stock & approvisionnement",
    "Responsable régional",           # Thiès
    "Chef de cuisine", "Responsable livraison", "Responsable service client", "Responsable stock & approvisionnement", "Chargé RH régional",
    "Responsable régional",           # Saint-Louis
    "Chef de cuisine", "Responsable livraison", "Responsable service client", "Responsable stock & approvisionnement", "Chargé RH régional"
  ),
  sexe = c("M", "M", "F", "M", "M", "M", "F", "M", "M", "M", "F", "M", "M", "M", "F", "M", "M", "M"),
  region = c(rep("Dakar", 6), rep("Thiès", 6), rep("Saint-Louis", 6)),
  photo = paste0("a (", 1:18, ")", ".jpg"),
  avis = round(runif(18, 3.5, 5), 2),
  age = sample(30:55, 18, replace = TRUE),
  nationalite = rep(c("Sénégalaise", "Camerounaise"), 9),
  salaire = sample(300000:800000, 18, replace = TRUE),
  contrat = sample(c("CDI", "CDD", "Stage", "Freelance"), 18, replace = TRUE),
  description = paste("Description du poste", 1:18),
  stringsAsFactors = FALSE
)


# === Noeuds ===
data$label <- paste(data$prenom, data$nom, "\n", data$poste)
data$shape <- "image"
data$image <- paste0("photos/", data$photo)
nodes <- data %>%
  #select(id, prenom, nom, poste, region, image, shape, sexe, salaire, description, nationalite) %>%
  mutate(
    label = paste0(prenom, " ", nom, "\n", poste, "\n", region),
    title = paste0(
      "<div style='display: flex; align-items: center;'>
        <img src='", image, "' style='width:50px; height:50px; border-radius:5px; margin-right:10px;'/>
        <div>
          <b>", prenom, " ", nom, "</b><br/>
          <i>", poste, "</i><br/>
          <span>", region, "</span>
        </div>
      </div>"
    ),
    color.background = ifelse(sexe == "F", "#FFD1DC", "#D6EAF8"),
    color.border = "#444"
  )


# === Édges généraux (tous les liens hiérarchiques) ===
edges <- data.frame(from = integer(0), to = integer(0))

# Lien DG → Responsables régionaux
dg_id <- data$id[data$poste == "Directeur Général"]
regional_ids <- data$id[data$poste == "Responsable régional"]

edges <- rbind(edges, data.frame(from = dg_id, to = regional_ids))

# Lien Responsables régionaux → subordonnés de leur région
for (r_id in regional_ids) {
  region_name <- data$region[data$id == r_id]
  sub_ids <- data$id[data$region == region_name & !(data$poste %in% c("Responsable régional", "Directeur Général"))]
  edges <- rbind(edges, data.frame(from = r_id, to = sub_ids))
}

# Fonction pour les cards a l'accueil
card_accueil <- function(card_list){
  bs4Card(
    width = 3,
    elevation = 2,
    status = card_list$color,
    class = "hover-zoom",
    div(
      style = "width: 100%;",
      div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        div(
          style = "flex-grow: 1;",
          tags$p(card_list$subtitle, style = "margin: 0;  font-size: 14px;"),
          tags$h4(card_list$val, style = "margin: 0; font-weight: bold; font-size: 22px;")
        ),
        tags$i(class = sprintf("fas fa-%s fa-2x", card_list$icon), style = "opacity: 0.3;"),
      )
    ),
    footer = div(
      tags$small("Évolution récente"),
      sparkline(card_list$spark, type = "line", width = "100%", height = "30px")
    )
  )
}

# Fonction pour les values box
vbox_fun <- function(val, titre, icon, col) {
  bs4ValueBox(
    value = val,
    subtitle = titre,
    icon = icon(icon),
    color = col
  )
}

# UI
ui <- bs4DashPage(
  title = tagList(
    icon("seedling"),  # Font Awesome
    "Tchop & Yamo Dashboard"
  ),
  header = bs4DashNavbar(
    skin = "light",
    status = "cyan",
    border = TRUE,
    sidebarIcon = icon("bars"),
    # Defnit le type d'icone superieure droite pour le controle du drawer de droite
    controlbarIcon = icon("sliders-h"), 
    # Ajouter le logo et le titre avec du style
    title = tagList(
      # Logo avec un style plus soigné
      img(src = "logo.jpg", height = "60px", style = "margin-top: 5px; margin-right: 10px; object-fit: cover;"),
      # Titre stylisé
      tags$span(style = "font-size: 24px; font-weight: bold; color: #333; margin-right: 15px; line-height: 50px;", "Tchop et Yamo")
    )
  ),
  sidebar = bs4DashSidebar(
    skin = "cyan",
    status = "primary",
    title = "AgriDash",
    brandColor = "primary",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Accueil", tabName = "home", icon = icon("home")),
      bs4SidebarMenuItem("Livraison", tabName = "livraison", icon = icon("truck")),
      bs4SidebarMenuItem("Repas", tabName = "repas", icon = icon("utensils")),
      bs4SidebarMenuItem("Employés", tabName = "employes", icon = icon("users")),
      bs4SidebarMenuItem("Gestion salaires", tabName = "masseSalariale", icon = icon("money-bill-wave")),
      bs4SidebarMenuItem("Organigramme", tabName = "organigramme", icon = icon("sitemap")),
      bs4SidebarMenuItem("Assistant IA", tabName = "assistantIA", icon = icon("robot"))
    )
  ),
  body = bs4DashBody(
    
    tags$head(
      
     # tags$link(rel = "stylesheet", href = "https://use.fontawesome.com/releases/v5.8.1/css/all.css"),
      tags$link(rel = "stylesheet", href = "https://use.fontawesome.com/releases/v5.15.4/css/all.css"),
      tags$style(HTML("
    .hover-zoom:hover {
      transform: scale(1.02);
      transition: transform 0.2s ease-in-out;
      cursor: pointer;
    }
    .card {
      transition: transform 0.2s ease-in-out;
    }
    
    .plat-container {
          border: 1px solid #ccc;
          border-radius: 10px;
          padding: 10px;
          margin-bottom: 15px;
          background-color: #f9f9f9;
          text-align: center;
        }
        .plat-image {
          width: 170px;
          height: 120px;
          object-fit: cover;
          border-radius: 8px;
        }
        .fa-star {
          color: yellow;
          margin: 0 3px;
        }

  
  "))
    ),
    
    bs4TabItems(
      # Page d'accueil avec carrousel
      bs4TabItem(
        tabName = "home",
        fluidRow(
          bs4Card(
            title = "Bienvenue chez Tchop et Yamo",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            # caroussel
            bs4Carousel(
              id = "accueil_carrousel",
              width = 12,
              bs4CarouselItem(
                active = TRUE,
                tags$img(src = "caroussel4.jpg", class = "d-block w-100", style = "height: 500px; object-fit: cover; width: 100%;"),
                tags$div(
                  class = "carousel-caption d-none d-md-block",
                  tags$h4("Cuisine de bonne qualité"),
                  tags$p("Nous vous offrons le meilleur, car nous savons que vous êtes les meilleurs.")
                )
              ),
              bs4CarouselItem(
                tags$img(src = "caroussel5.jpg", class = "d-block w-100", style = "height: 500px; object-fit: cover; width: 100%;"),
                tags$div(
                  class = "carousel-caption d-none d-md-block",
                  tags$h4("Accessibilité partout"),
                  tags$p("Nos points de ventes répartis dans toutes les régions du pays.")
                )
              ),
              bs4CarouselItem(
                tags$img(src = "caroussel6.jpg", class = "d-block w-100", style = "height: 500px; object-fit: cover; width: 100%;"),
                tags$div(
                  class = "carousel-caption d-none d-md-block",
                  tags$h4("Service Rapide"),
                  tags$p("Commandez et faites vous livrer en quelques minutes.")
                )
              )
            )
          )
        ),
        
        # Filtres
        tags$div(style="height: 15px; width: 100%"),
        selectInput("choixRegion", "Choisir les régions", choices = names(regions_villes), multiple = TRUE),
        
        tags$div(style="height: 15px; width: 100%"),
        tags$h3("Summary"),
        
        # Cartes de la pages accueil
        uiOutput("cards_acceuil"),
        
        
        fluidRow(
          box(width = 6, title = "Carte des chiffres d'affaires", leafletOutput("map_accueil", height = 400)),
          box(width=6, title = "Masse salariale par poste", plotlyOutput("salaire_postes"))
          
        ),
        tags$div(style="height: 15px; width: 100%"),
        fluidRow(
          box(width=5, title = "Employés par sexe", plotlyOutput("sexe_employes", width = "100%")),
          #box(width=3, title = "Ventes par catégorie", plotlyOutput("vente_categories")),
          box(width = 7, title = "Liste des employés", DTOutput("table_employes"))
        ),
        
      ),
      
      
      
      # Organigramme
      bs4TabItem(
        tabName = "organigramme",
        #h3("Organigramme des structures par région"),
        
        div(
          style = "background: linear-gradient(90deg, #007bff, #00c6ff); padding: 25px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);",
          h1(
            tags$i(class = "fas fa-users", style = "margin-right: 15px;"),
            "TCHOP ET YAMO – Organigramme des structures par région",
            style = "color: white; font-size: 32px; font-weight: 600; text-align: center; margin: 0;"
          )
        ),
        
        selectInput("region", "Sélectionner une région :", 
                    choices = unique(data$region), selected = "Dakar"),
        visNetworkOutput("network", height = "800px"),
        
        uiOutput("detailsModal")
      
    ),
    
    # Assistant IA
    bs4TabItem(
      tabName = "assistantIA",
      
      #selectInput("choixModel","Choisir le modèle",choices = c("Groq","Deepseek"), selected = "Groq"),
      
      fluidRow(
        bs4Card(
          title = "Assistant IA - Posez votre question",
          width = 12,
          status = "warning",
          solidHeader = TRUE,
          
          tags$style(HTML("
        .chat-container {
          overflow-y: auto;
          height: 400px;
          padding: 10px;
          background: #f9f9f9;
          border: 1px solid #ccc;
          margin-bottom: 15px;
        }
        .user-chat, .bot-chat {
          margin: 10px 0;
          white-space: pre-wrap;
          padding: 10px 14px;
          border-radius: 8px;
          line-height: 1.5;
        }
        .user-chat {
          background: #e3f2fd;
          text-align: right;
        }
        .bot-chat {
          background: #fff8e1;
          text-align: left;
        }
      ")),
          
          div(
            style = "background: linear-gradient(90deg, #007bff, #00c6ff); padding: 25px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);",
            h1(
              tags$i(class = "fas fa-robot", style = "margin-right: 15px;"),
              "TCHOP ET YAMO – Assistant IA",
              style = "color: white; font-size: 32px; font-weight: 600; text-align: center; margin: 0;"
            )
          ),
          
          uiOutput("chat_ui"),
          
          textAreaInput(
            inputId = "q",
            label = "Votre question :",
            placeholder = "Tapez ici votre question...",
            width = "100%",
            height = "80px"
          ),
          
          actionButton(
            inputId = "send",
            label = "Envoyer",
            class = "btn btn-primary"
          )
        )
      )
    ),
    
    # Livraison
    bs4TabItem(
      tabName = "livraison",
      div(
        style = "background: linear-gradient(90deg, #007bff, #00c6ff); padding: 25px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);",
        h1(
          tags$i(class = "fas fa-users", style = "margin-right: 15px;"),
          "TCHOP ET YAMO – Gestion des livraisons",
          style = "color: white; font-size: 32px; font-weight: 600; text-align: center; margin: 0;"
        )
      ),  
      # integration de l'ui
      livraison_ui("livraison1")
    ),
    
    # Repas
    bs4TabItem(
      tabName = "repas",
      div(
        style = "background: linear-gradient(90deg, #007bff, #00c6ff); padding: 25px; border-radius: 12px; margin-bottom: 30px; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);",
        h1(
          tags$i(class = "fas fa-users", style = "margin-right: 15px;"),
          "TCHOP ET YAMO – Suivi des plats",
          style = "color: white; font-size: 32px; font-weight: 600; text-align: center; margin: 0;"
        )
      ),  
      # integration de l'ui
      repas_ui("repas1")
    ),
    
    # Employes
    bs4TabItem(
      tabName = "employes",
      # integration de l'ui
      employes_ui("employes1")
    ),
    
    # Masse salariale 
    
    bs4TabItem(
      tabName = "masseSalariale",
      # integration de l'ui
      salaire_ui("salaires1")
    )
    
  ),
  
  # Icone juste en haut a droite pour le drawer de droite
  controlbar = bs4DashControlbar(
    skin = "light",
    title = "Options",
    collapsed = TRUE,
    controlbarMenu(
      
      # Onglet Filtres avancés
      controlbarItem(
        title = "Filtres avancés",
        icon = icon("sliders-h"),
        selectInput("type_commande", "Type de commande",
                    choices = c("Toutes", "Sur place", "À emporter", "Livraison")),
        #selectInput("choixRegion", "Choisir les régions", choices = names(regions_villes), multiple = TRUE),
        selectInput("choixVille","Choisir les villes", choices = NULL, multiple = TRUE),
        dateRangeInput("date","Choisir une période", start = Sys.Date() - 30, end = Sys.Date()),
        selectInput("choixCategorie","Choisir les catégories", choices = c("Repas","Boissons","Apperitifs","Divers"), multiple = TRUE)
      ),
      
      # Onglet Résumé de l'activité
      controlbarItem(
        title = "Résumé",
        icon = icon("chart-pie"),
        bs4ValueBoxOutput("nb_commandes", width = 12),
        bs4ValueBoxOutput("ca_total", width = 12),
        bs4ValueBoxOutput("panier_moyen", width = 12)
      ),
      
      # Onglet Informations
      controlbarItem(
        title = "Infos générales",
        icon = icon("info-circle"),
        tags$p("Application de suivi des performances régionales."),
        tags$p("Données : commandes, chiffre d'affaires, bénéfice net."),
        tags$p("Filtres disponibles : région, ville, période, etc.")
      )
    )
  ),
  ), # fin body
  
  # Footer
  footer = bs4DashFooter(
    left = "© 2025 Tchop & Yamo",
    right = "Conçu avec bs4Dash"
  )
)

# Server
server <- function(input, output, session) {
  
  selected_region <- reactive({
    input$choixRegion
  })
  
  # Mettre à jour les options de villes
  observeEvent(input$choixRegion, {
    villes_filtrees <- unique(unlist(regions_villes[input$choixRegion]))
    updateSelectInput(session, "choixVille", choices = villes_filtrees)
  })
  
  # Controlbar
  
  
  # Onglet Accueil
  base_employes <- readxl::read_excel("Data/employes.xlsx")
  
  df_employes <- reactive({
    if (is.null(selected_region()) || length(selected_region()) == 0) {
      base_employes
    } else {
      base_employes %>% filter(région %in% selected_region())
    }
  })
  
  # On crée une UI dynamique pour les cards
  nb_employes <- reactive({
    nrow(df_employes())
  })
  
  masse_salariale <- reactive({
    format_abbr(sum(df_employes()[,"salaire"]))
  })
  
  chiffre_affaire <- reactive({
    format_abbr(
      sum(df_employes()[,"salaire"]) * 1.8
    )
  })
  
  benefice_net <- reactive({
    format_abbr(
      sum(df_employes()[,"salaire"]) * 0.6
    )
  })
    
  output$cards_acceuil <-  renderUI({
    fluidRow(
      lapply(list(
        list(val = paste(chiffre_affaire(),"FCFA",sep=" "), subtitle = "Chiffre d'affaires", icon = "chart-line", color = "primary", spark = c(80, 100, 120, 115, 130)),
        list(val = paste(masse_salariale(),"FCFA",sep=" "), subtitle = "Masse salariale", icon = "money-bill-wave", color = "success", spark = c(35, 38, 39, 40, 41)),
        list(val = paste(benefice_net(),"FCFA",sep=" "), subtitle = "Bénéfice net", icon = "wallet", color = "info", spark = c(70, 72, 75, 78, 80)),
        list(val = nb_employes(), subtitle = "Employés", icon = "users", color = "warning", spark = c(100, 105, 110, 115, 120)),
        list(val = "70", subtitle = "Points de vente", icon = "store", color = "info", spark = c(50, 55, 60, 65, 70))
      ), function(card_list) {
        card_accueil(card_list = card_list)
      })
    )
  })
  
  
  # Préparation des données pour la masse salariale
  salaire_poste <- reactive({
    df_employes() %>%
      group_by(poste) %>%
      summarise(masse_salariale = sum(salaire, na.rm = TRUE)) %>%
      arrange(desc(masse_salariale)) %>%
      mutate(postes = factor(poste, levels = unique(poste))) %>%
      ungroup()
  })
  
  # Palette de couleurs (autant que nécessaire)
  couleurs <- reactive({
    RColorBrewer::brewer.pal(n = max(3, nrow(salaire_poste())), name = "Set3")
  })
  
  output$salaire_postes <- renderPlotly({
    plot_ly(
      data = salaire_poste(),
      x = ~masse_salariale,
      y = ~postes,
      type = "bar",
      orientation = 'h',
      marker = list(color = couleurs())
    ) %>%
      layout(
        title = list(text = "", x = 0.5),
        showlegend = FALSE,
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        margin = list(l = 60, r = 20, b = 30, t = 40, pad = 0)
      )
  })
  
  # Compter les sexes
  sexe_count <- reactive({
    df_employes() %>%
      count(sexe) %>%
      mutate(sexe = ifelse(sexe == "M", "Masculin", "Féminin"))
  })  
  
  # Affichage dans le UI
  output$sexe_employes <- renderPlotly({
    plot_ly(data = sexe_count(), 
            labels = ~sexe, 
            values = ~n, 
            type = 'pie', 
            hole = 0.6) %>%
      layout(title = "",
             showlegend = TRUE,
             paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)',
             margin = list(l = 0, r = 0, b = 0, t = 30, pad = 0),
             autosize = TRUE)
  })
  
  # Visualisations sur la carte
  region_sf <-  st_read("Data/SEN_adm1/SEN_adm1.shp", quiet = TRUE)
  
  salaire_regions <- base_employes %>%
      group_by(région) %>%
      summarise(masse_salariale = sum(salaire, na.rm = TRUE))

  
  # Appariement avec la couche géographique
  regions_sf <- left_join(region_sf, salaire_regions, by = c("NAME_1" = "région"))

  
  # Carte interactive
  output$map_accueil <- renderLeaflet({
    pal <- colorQuantile("YlGnBu", domain = regions_sf$masse_salariale, n = 5)
    
    leaflet(data = regions_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(masse_salariale),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste0("Région : ", NAME_1, "Chiffre d'affaire : ", format(masse_salariale, big.mark = " ", scientific = FALSE), " FCFA"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal, values = ~masse_salariale, opacity = 0.7, title = "Chiffre d'affaire", position = "bottomright")
  })
  
  
  
  output$table_employes <- renderDataTable({
    df_employes()[, c("nom","prenom","sexe", "région", "salaire", "poste")]
  })
  
  
  # Organigramme
  # regional_ids <- reactive({
  #   regions %>% filter(poste == "Responsable régional", region == input$region) %>% pull(id)
  # })
  regional_ids <- reactive({
    data %>%
      filter(poste == "Responsable régional", region == input$region) %>%
      pull(id)
  })
  
 
  
  #================
  # Filtrage des noeuds à afficher
  nodes_region <- reactive({
    equipe_ids <- data$id[data$region == input$region]
    dg_id <- data$id[data$poste == "Directeur Général"]
    
    data %>%
      filter(id %in% c(dg_id, equipe_ids)) %>%
      mutate(
        label = paste0(prenom, " ", nom, "\n", poste),
        shape = "image",
        image = paste0("photos/", photo),
        title = paste0("<b>", prenom, " ", nom, "</b><br>", poste, "<br><i>", description, "</i>"),
        color.background = ifelse(sexe == "F", "#FFD1DC", "#D6EAF8"),
        color.border = "#2C3E50"
      ) %>%
      select(id, label, shape, image, title, color.background, color.border)
  })
  
  # Filtrage des liens à afficher
  edges_region <- reactive({
    dg_id <- data$id[data$poste == "Directeur Général"]
    responsables <- regional_ids()
    
    e1 <- edges %>% filter(from == dg_id & to %in% responsables)
    e2 <- edges %>% filter(from %in% responsables)
    bind_rows(e1, e2)
  })
  #==========
  
  
  output$network <- renderVisNetwork({
    visNetwork(nodes_region(), edges_region(), height = "700px") %>%
      visNodes(shapeProperties = list(useBorderWithImage = TRUE, useImageSize = FALSE), size=70, shape = "box",  font = list(multi = TRUE, size = 16)) %>%
      visEdges(arrows = "to") %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE),
        nodesIdSelection = TRUE
      ) %>%
      visLayout(hierarchical = list(direction = "UD", sortMethod = "directed", levelSeparation = 300)) %>%
      visInteraction(navigationButtons = TRUE, zoomView = TRUE) %>%
      visPhysics(stabilization = TRUE) %>%
      visEvents(selectNode = "function(nodes) {
      Shiny.setInputValue('selected_node', nodes.nodes[0]);
    }")
  })
  
  output$detailsModal <- renderUI({
    req(input$selected_node)
    
    employe <- data %>% filter(id == input$selected_node)
    
    showModal(modalDialog(
      title = paste("Détails sur", employe$prenom, employe$nom),
      fluidRow(
        column(4, img(src = employe$image, width = "100%")),
        column(8,
               tags$b("Poste : "), employe$poste, tags$br(),
               tags$b("Région : "), employe$region, tags$br(),
               tags$b("Sexe : "), employe$sexe, tags$br(),
               tags$b("Age : "), employe$age, tags$br(),
               tags$b("Description : "), employe$description, tags$br(),
               tags$b("Salaire : "), paste(employe$salaire,"FCFA",sep=" "), tags$br(),
        )
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  # Assistant IA
  #modele <- reactive({
   # input$choixModel
  #})
  
  #====================== CROQ API =================================
  call_groq_api <- function(history) {
    api_key = get_api("Groq")
    if (identical(api_key, "")) {
      stop("Merci de définir la variable d'environnement GROQ_API_KEY avec votre clé Groq.")
    }
    
    resp <- httr::POST(
      url = "https://api.groq.com/openai/v1/chat/completions",
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(
        list(
          model       = "llama3-8b-8192",
          messages    = history,
          temperature = 0.7
        ),
        auto_unbox = TRUE
      ),
      encode = "json"
    )
    
    if (httr::status_code(resp) != 200) {
      return("Erreur lors de l’appel à l’API Groq.")
    }
    
    parsed <- httr::content(resp, "parsed", encoding = "UTF-8")
    parsed$choices[[1]]$message$content
  }
  
  format_markdown_console <- function(md_text) {
    md_text <- gsub("^#+\\s*(.*)", toupper("\\1"), md_text)
    md_text <- gsub("\\*\\*(.*?)\\*\\*", toupper("\\1"), md_text)
    md_text <- gsub("\\*(.*?)\\*", "\\1", md_text)
    md_text <- gsub("^\\s*\\-\\s*", "• ", md_text)
    md_text <- gsub("```", "", md_text)
    md_text <- gsub("`([^`]*)`", "'\\1'", md_text)
    gsub("\n{2,}", "\n", md_text)
  }
  #=============================
  
  
  #================= API deepseek ===============
  call_deepseek_api <- function(history) {
    api_key = get_api("deepseek")
    if (identical(api_key, "")) {
      stop("Merci de définir la variable d'environnement DEEPSEEK_API_KEY avec votre clé DeepSeek.")
    }
    
    resp <- httr::POST(
      url    = "https://openrouter.ai/api/v1/chat/completions",
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ),
      body   = jsonlite::toJSON(
        list(
          model     = "deepseek/deepseek-chat-v3-0324:free",
          messages  = history,
          temperature = 0.7
        ),
        auto_unbox = TRUE
      ),
      encode = "json"
    )
    
    if (httr::status_code(resp) != 200) {
      return(paste("Erreur DeepSeek API",httr::status_code(resp)))
    }
    
    parsed <- httr::content(resp, "parsed", encoding = "UTF-8")
    parsed$choices[[1]]$message$content
  }
  
  
  # R/utils.R
  
  #' @noRd
  format_markdown_console <- function(md_text) {
    md_text <- gsub("^#+\\s*(.*)", toupper("\\1"), md_text)
    md_text <- gsub("\\*\\*(.*?)\\*\\*", toupper("\\1"), md_text)
    md_text <- gsub("\\*(.*?)\\*", "\\1", md_text)
    md_text <- gsub("^\\s*\\-\\s*", "• ", md_text)
    md_text <- gsub("```", "", md_text)
    md_text <- gsub("`([^`]*)`", "'\\1'", md_text)
    gsub("\n{2,}", "\n", md_text)
  }
  #====================================
  
  
  
  chat_history <- reactiveVal(list())
  
  observeEvent(input$send, {
    req(input$q)
    
    history <- chat_history()
    history <- c(history, list(list(role = "user", content = input$q)))
    
    # Appel du modele
    res <- call_groq_api(history)
    #if(modele()=="Groq"){
     # res <- call_groq_api(history)
    #} else {
     # res <- call_deepseek_api(history)
    #}
    
    
    history <- c(history, list(list(role = "assistant", content = res)))
    chat_history(history)
    
    updateTextAreaInput(inputId = "q", value = "")
  })
  
  output$chat_ui <- renderUI({
    msgs <- chat_history()
    if (length(msgs) == 0) return(NULL)
    
    elems <- lapply(msgs, function(msg) {
      div(class = if (msg$role == "user") "user-chat" else "bot-chat",
          format_markdown_console(msg$content))
    })
    div(class = "chat-container", elems)
  })
  
  # Livraison
  livraison_server("livraison1")  # Appel du module avec l'ID "livraison1"
  
  # Repas
  repas_server("repas1")
  
  # Employes
  employes_server("employes1")
  
  # Masse salariale
  salaire_server("salaires1")
}

# Run the application 
shinyApp(ui = ui, server = server)
