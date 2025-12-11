
library(shiny)  # Framework pour construire des applications web interactives
library(shinythemes)  # Thèmes pour l'interface utilisateur de Shiny
library(shinyWidgets)  # Widgets supplémentaires pour l'interface utilisateur
library(DT)  # Pour afficher des tables interactives
library(skimr)  # Pour des résumés statistiques des données
library(ggplot2)  # Pour la création de graphiques
library(RColorBrewer)
library(viridis) 
# (install.packages("rsconnect"))
# library(rsconnect)

# package pour les modèles
library(plotrix)  # Outils graphiques pour l'analyse de données
library(MASS)  # Pour des fonctions statistiques
library(klaR)  # Pour des algorithmes de classification
library(e1071)  # Pour SVM et autres méthodes
library(pROC)
library(gbm)  # Pour le gradient boosting
library(ROCR)  # Pour l'évaluation de performance des modèles
require(caret)  # Pour le prétraitement des données et l'évaluation des modèles
library(scales)  # Assurez-vous d'avoir le package scales
# Chargement des données de voitures Hyundai à partir d'un fichier CSV

hyundai <- read.csv("hyundi.csv")
hyundai$transmission <- as.factor(hyundai$transmission)
hyundai$model <- as.factor(hyundai$model)
hyundai$fuelType <- as.factor(hyundai$fuelType)
modele_prediction <- lm(price ~ year + mileage + transmission + model + fuelType + mpg + engineSize, data = hyundai)


# Définition des variables numériques et des variables explicatives
noms_var_num <- c("year", "price", "mileage", "tax", "mpg", "engineSize")  # Variables numériques
lv.xvars <- c("price", "model", "year", "mileage", "mpg", "engineSize", "transmission", "fuelType")  # Variables explicatives
variable_labels <- c( "price" = "Prix", "model" = "Modèle", "year" = "Année", "mileage" = "Kilométrage",
                      "mpg" = "Consommation (mpg)", "engineSize" = "Taille du moteur",
                      "transmission" = "Transmission", "fuelType" = "Type de carburant")
variable_labels1 <- c( "model" = "Modèle", "year" = "Année", "mileage" = "Kilométrage",
                      "mpg" = "Consommation (mpg)", "engineSize" = "Taille du moteur",
                      "transmission" = "Transmission", "fuelType" = "Type de carburant")
choix_transmission <- c("Automatique" = "Automatic", "Manuelle" = "Manual", 
                        "Semi-automatique" = "Semi-Auto", "Autre" = "Other")
choix_carburant <- c("Essence" = "Petrol", "Diesel" = "Diesel", 
                     "Hybride" = "Hybrid", "Autre" = "Other")


# Définition de l'interface utilisateur (UI) de l'application
ui <-  fluidPage(
  tags$head(
    tags$style(HTML("
      /* Style pour les notifications */
      .shiny-notification {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 9999;  /* Pour s'assurer que la notification est au-dessus des autres éléments */
      }

      /* Style pour l'arrière-plan de l'onglet4 */
      #background-section {
        position: relative;
      }
      
      #background-section::before {
        content: '';
        background-image: url('fond_decran.jpeg');
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        opacity: 0.7; /* 30% de transparence */
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        z-index: -1; /* Place l'image derrière le contenu */
      }
    "))
  ),
  
  theme = shinytheme("flatly"), # Application d'un thème à l'interface utilisateur
  titlePanel(tags$div(
    style = "display: inline; align-items: center; margin-top: 10px;",  # Utilisez : pour définir la marge
    tags$h2("Exploration du jeu de données HYUNDAI", style = "display: inline; margin: 0;"),  # Titre principal sans marge
    tags$span("par Thierry F. Saadem", style = "font-size: 15px; font-style: italic; display: inline; margin: 0;")  # Nom de l'auteur sans marge
  )),  # Titre de l'application
  navbarPage(   # Titre de l'application
    title = "Navigation visuelle :",
    tabPanel(       # Onglet d'accueil avec une image et un lien
      title = icon("home"),
      img(src = 'compa1.jpg', width = "100%"),
      p("Hyundai Company", tags$a(
        href = "https://www.cgtrader.com/gallery/project/hyundai-car-showroom", 
        "https://www.cgtrader.com/gallery/project/hyundai-car-showroom"
      )),
      tags$audio(src = "Earth1.mp3", type = "audio/mp3", autoplay = TRUE, loop = TRUE)
    ),
    tabPanel(    # Onglet pour afficher les données brutes
      title = "Données brutes",
      DT::dataTableOutput("table_donnees") # Table interactive pour afficher les données
    ),
    tabPanel(
      title = "Statistiques descriptives",     # Onglet pour les statistiques descriptives
      p("Structure de l'objet R contenant les données :"),
      verbatimTextOutput("sortie_str"),  # Affiche la structure des données
      HTML("<br>"),  # Ajoute deux lignes vides
      p("Statistiques descriptives pour une variable sélectionnée :"),
      sidebarLayout(  # Disposition de l'interface avec un panneau latéral et un panneau principal
        sidebarPanel = sidebarPanel(
          selectInput(
            inputId = "variable",  # ID de l'entrée
            label = "Choisissez une variable :",  # Étiquette de l'entrée
            choices = setNames(names(hyundai[, c("price", "model", "year", "mileage",  "mpg",
                                                 "engineSize", "transmission", "fuelType")]), 
                               variable_labels), selected = "year"
          ),
          img(src = "interior_genesis.jpg", width = "100%", height = "100%")  # Image dans le panneau latéral
        ),
        mainPanel = mainPanel(
          htmlOutput("type_var"),  # Affiche le type de variable sélectionnée
          tableOutput("sortie_stat_desc")  # Table pour afficher les statistiques descriptives
        )
      )
    ),
    
    ############""
    tabPanel( # Onglet pour afficher des graphiques
      title = "Graphiques",
      sidebarLayout(
        sidebarPanel(
          img(src = 'Hyundai_Platform.jpg', width = "100%"), # Image dans le panneau latéral
          p("Hyundai_Platform", 
            tags$a(
              href = "https://www.autonomous.gr/hyundai-group-tha-xekinisei-ti-diki-tis-platforma-ilektrikon-aytokiniton/", 
              "https://www.autonomous.gr/hyundai-group-tha-xekinisei-ti-diki-tis-platforma-ilektrikon-aytokiniton/"
            )
          ) # Lien associé à l'image
        ),
        
        mainPanel(
          # Sous-onglet 1 : Relation entre le prix et une variable
          tabsetPanel(
            tabPanel(
              "Comment varie le prix en fonction d'une variable", # Sélection d'une variable
              sidebarLayout(
                sidebarPanel(width = 12,
                  selectInput("var_single", "Choisissez une variable :",
                              choices = setNames(names(hyundai[, c("model", "year", "mileage", "mpg", "engineSize", "transmission", "fuelType")]), 
                                                 variable_labels1), selected = "transmission")
                ),
                mainPanel(
                  # Ajouter une marge en haut avec CSS
                  div(style = "margin-top: 50px; margin-left:80px;",  plotOutput("plot_single",width = "1000px", height = "600px"))
                )
              )
            ),
            
            # Sous-onglet 2 : Relation entre le prix et deux variables
       ####     
            tabPanel(
  "Comment varie le prix en fonction de deux variables",   # Sélection de deux variables
  sidebarLayout(
    sidebarPanel(width = 12,
      selectInput("var_double1", "Choisissez la première variable :", 
                  choices = setNames(names(hyundai[, c("model", "year", "mileage", "mpg", "engineSize", "transmission", "fuelType")]), 
                                     variable_labels1), selected = "year"),
      selectInput("var_double2", "Choisissez la deuxième variable :", 
                  choices = setNames(names(hyundai[, c("model", "year", "mileage", "mpg", "engineSize", "transmission", "fuelType")]), 
                                     variable_labels1), selected = "mileage")
    ),
        # Affichage du graphique avec marge
        mainPanel(
        div(style = "margin-top: 50px; margin-left:80px;",  # Forcer l'occupation maximale de la largeur
        plotOutput("plot_double", width = "1000px", height = "500px"))
        )
  )
)

        
          )
        )
      )
    )
    ,
 #################################   
    tabPanel(    # Onglet pour l'estimation du prix des véhicules
      title = "Estimation du prix des véhicules",
      div(id = "background-section",  # Appliquer l'ID pour ajouter le style de fond
 
      sidebarLayout(
        sidebarPanel( width = 4,
                     style = "height: 820px",
          numericInput("year", "Année:", value = 2018, min = 2000, max = 2023),
          numericInput("mileage", "Kilométrage:", value = 30000, min = 0, max = 220000, step = 1000),
          radioButtons("transmission", "Transmission:", 
                      choices = choix_transmission, 
                      selected = choix_transmission["Automatique"],
                      inline = TRUE),
          selectInput("model", "Modèle:", 
                      choices = levels(hyundai$model), selected = levels(hyundai$model)[1]),
          radioButtons("fuelType", "Type de carburant:",
                       choices = choix_carburant,
                       selected = choix_carburant["Essence"],
                       inline = TRUE),
          numericInput("mpg", "Consommation (mpg):", value = 50, min = 0),
          
          numericInput("engineSize", "Taille du moteur (L):", value = 1.6, min = 0, step = 0.1),
          div(style = "text-align: center; margin-top: 50px;", 
              actionButton("predict", "Cliquer ici pour avoir une estimation du prix du véhicule", class = "btn btn-info"))
        ),
        mainPanel(
          htmlOutput("result")
          
        ) )
      )
    )
  )
)


# Définir la logique du serveur pour l'application Shiny
server <- function(input, output){
  # Rendre un tableau de données des voitures Hyundai avec des options de téléchargement
  output$table_donnees <- DT::renderDataTable({
    tmp.title<-'BaseDonnees_hyundai' # Titre pour le téléchargement
    DT::datatable(hyundai,  # Source de données
                  extensions = 'Buttons',escape=TRUE,
                  options = list(
                    pageLength = 50,  # Nombre de lignes à afficher par page
                    dom = 'Blfrtip',  # Structure DOM pour le tableau
                    buttons = list(# Boutons de téléchargements
                      list(extend = 'csv', title = tmp.title),
                      list(extend = 'excel', title = tmp.title),
                      list(extend = 'pdf', title = tmp.title),
                      list(extend = 'colvis', text = 'Choix des colonnes'),
                      list(extend = 'print', text = 'Imprimer', title = tmp.title)
                    )
                  )) 
  })
  
  output$sortie_str <- renderPrint({str(hyundai)  # Afficher la structure de l'ensemble de données
  })
  
  output$type_var <- renderText({
    # Récupère la traduction française de la variable sélectionnée
    variable_fr <- variable_labels[[as.character(input$variable)]]
    
    
    if (as.character(input$variable) %in% c("model", "transmission", "fuelType")) {
      HTML(paste("<br><br><br><br>Fréquences des modalités observées de la variable :", variable_fr))
    } else {
      HTML(paste("<br><br><br><br>Indicateurs de dispersion et de tendance centrale de la variable :", variable_fr))
    }
  })
  
  # Rendre des statistiques descriptives ou les fréquences selon le type de variable
  output$sortie_stat_desc <- renderTable({
    if (as.character(input$variable) %in% c("model", "transmission", "fuelType")) {
      t(as.matrix(table(hyundai[[input$variable]], useNA = "ifany"))) # Tableau de fréquence pour les variables catégoriques
    } else {
      t(as.matrix(summary(hyundai[[input$variable]]))) # Statistiques sommaires pour les variables numériques
    }
  })
  ######

  output$plot_single <- renderPlot({
    # Vérifier si la variable est numérique ou catégorielle
    variable_fr1 <- switch(input$var_single, "model" = "modèle", "year" = "année", "mileage" = "kilométrage",
                           "mpg" = "Consommation (mpg)", "fuelType" = "type de carburant", 
                           "transmission" = "transmission", "engineSize" = "taille du moteur")
    if (is.numeric(hyundai[[input$var_single]])) {
      # Scatterplot avec jitter pour les variables numériques 
        ggplot(hyundai, aes_string(x = input$var_single, y = "price",color="input$var_single")) +
        geom_jitter(size = 3, width = 0.25, height = 0.25, alpha = 0.5, show.legend = FALSE) +  # Ajuster la largeur, hauteur et opacité pour mieux séparer les points
        labs(title = paste("Prix en fonction de la variable", variable_fr1), 
        x = variable_fr1, y = "Prix en dollars") +
        scale_y_continuous(labels = comma) +  # Formatage des valeurs y sans notation scientifique
       # coord_cartesian(xlim = c(0, 60), ylim = c(0, 50000)) +  # Ajuster les limites selon les valeurs de nos données
        #scale_color_viridis(option = "D") +  # Palette de dégradé de viridis 
        scale_x_continuous(labels = comma) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20,  margin = margin(b = 20)),  # Titre en gras et centré
          axis.title.x = element_text(face = "bold", size = 15),  # Nom de l'axe x en gras
          axis.title.y = element_text(face = "bold", size = 15),  # Nom de l'axe y en gras
          axis.text.x = element_text( hjust = 1, face = "bold", size = 12),  # Étiquettes de l'axe x en gras et verticales
          axis.text.y = element_text(face = "bold" , size = 12)  # Étiquettes de l'axe y en gras
        )
    } else {
###      
      ggplot(hyundai, aes_string(x = input$var_single, y = "price", fill = input$var_single)) +
        geom_bar(stat = "summary", fun = "mean", na.rm = TRUE) +  
        labs(title = paste("Prix en fonction de la variable", variable_fr1), 
             x = variable_fr1, y = "Prix en dollars", fill = variable_fr1) +
        scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666",
                                     "#8c564b", "#2ca02c", "#ff7f0e", "#1f77b4", "#9467bd", "#d62728", "#bcbd22", "#17becf")) +  
        scale_y_continuous(labels = comma) +  # Formatage des valeurs y sans notation scientifique
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20,  margin = margin(b = 20)),  # Titre en gras et centré
          axis.title.x = element_text(face = "bold" , size = 15),  # Nom de l'axe x en gras
          axis.title.y = element_text(face = "bold" , size = 15),  # Nom de l'axe y en gras
          axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 12),  # Étiquettes de l'axe x en gras et verticales
          axis.text.y = element_text(face = "bold", size = 12)  # Étiquettes de l'axe y en gras
        )
      
    }
 #####   
  })
  
  ################# graphe 02  
  output$plot_double <- renderPlot({
    variable_fr2 <- switch(input$var_double1, "model" = "modèle", "year" = "année", "mileage" = "kilométrage",
                           "mpg" = "Consommation (mpg)", "fuelType" = "type de carburant",
                           "transmission" = "transmission", "engineSize" = "taille du moteur")
    variable_fr3 <- switch(input$var_double2, "model" = "modèle", "year" = "année", "mileage" = "kilométrage",
                           "mpg" = "Consommation (mpg)", "fuelType" = "type de carburant",
                           "transmission" = "transmission", "engineSize" = "taille du moteur")
    # Vérifier si les deux variables sont les mêmes
    if (input$var_double1 == input$var_double2) {
      showNotification("Choisissez une variable différente pour la deuxième variable", type = "error")
      return(NULL)  # Arrêter l'exécution de la fonction
    }
    
    # Vérifier si les variables sont numériques ou catégorielles
    var1_is_numeric <- is.numeric(hyundai[[input$var_double1]])
    var2_is_numeric <- is.numeric(hyundai[[input$var_double2]])
    
    var1_is_factor <- is.factor(hyundai[[input$var_double1]]) || is.character(hyundai[[input$var_double1]])
    var2_is_factor <- is.factor(hyundai[[input$var_double2]]) || is.character(hyundai[[input$var_double2]])
    
    # Si les deux variables sont catégorielles, afficher un barplot juxtaposé
    if (var1_is_factor && var2_is_factor) {
      ggplot(hyundai) +
        aes_string(x = input$var_double1, y = "price", color = input$var_double2) +
        #geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +  # Utiliser stat = "identity" pour un graphique avec une variable continue
        geom_boxplot() +
        scale_color_brewer(palette = "RdYlGn", direction = 1) +
        labs(title = paste("Distribution du prix en fonction des variables", variable_fr2 , "et", variable_fr3), 
             x = variable_fr2, y = "Prix en dollars", color = variable_fr3) +
        scale_y_continuous(labels = scales::comma) +  # Utiliser scales::comma pour formater les nombres
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20,  margin = margin(b = 20)),  # Titre en gras et centré
          axis.title.x = element_text(face = "bold", size = 15),  # Nom de l'axe x en gras
          axis.title.y = element_text(face = "bold", size = 15),  # Nom de l'axe y en gras
          axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 12),  # Étiquettes de l'axe x en gras et verticales
          axis.text.y = element_text(face = "bold", size = 12)  # Étiquettes de l'axe y en gras
          )  # Centrer le titre
      
    } 
    
    # Si les deux variables sont numériques, afficher un scatterplot
    else if (var1_is_numeric && var2_is_numeric) {
      ggplot(hyundai, aes_string(x = input$var_double1, y = "price", color = input$var_double2)) +
        #geom_(color = "blue", size = 2, alpha = 0.6) +  # Nuage de points
        geom_jitter() +
        scale_color_distiller(palette = "Accent", direction = 1) +
        labs(title = paste("Distribution du prix en fonction des variables", variable_fr2 , "et", variable_fr3), 
             x = variable_fr2, y = "prix en dollars", color = variable_fr3) +
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = comma) +
        theme(
              plot.title = element_text(hjust = 0.5, face = "bold", size = 20,  margin = margin(b = 20)),  # Titre en gras et centré
              axis.title.x = element_text(face = "bold", size = 15),  # Nom de l'axe x en gras
              axis.title.y = element_text(face = "bold", size = 15),  # Nom de l'axe y en gras
              axis.text.x = element_text( hjust = 1, face = "bold", size = 12),  # Étiquettes de l'axe x en gras et verticales
              axis.text.y = element_text(face = "bold", size = 12)  # Étiquettes de l'axe y en gras   
              )
    } 
    
    # Si une variable est numérique et l'autre est catégorielle, afficher un scatterplot avec couleur
    else if (var1_is_numeric && var2_is_factor) {
      # Palette de couleurs personnalisée
      ggplot(hyundai, aes_string(x = input$var_double1, y = "price", color = input$var_double2)) +
        geom_jitter() +  # Scatterplot avec jitter
        scale_color_hue(direction = 1) + 
        labs(title = paste("Distribution du prix en fonction des variables", variable_fr2 , "et", variable_fr3), 
             x = variable_fr2, y = "Prix en dollars", color = variable_fr3) +
        scale_x_continuous(labels = comma) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20,  margin = margin(b = 20)),  # Titre en gras et centré
          axis.title.x = element_text(face = "bold", size = 15),  # Nom de l'axe x en gras
          axis.title.y = element_text(face = "bold", size = 15),  # Nom de l'axe y en gras
          axis.text.x = element_text( hjust = 1, face = "bold", size = 12),  # Étiquettes de l'axe x en gras et verticales
          axis.text.y = element_text(face = "bold", size = 12)  # Étiquettes de l'axe y en gras   
        )
    } 
    
    # Si une variable est catégorielle et l'autre est numérique, afficher un scatterplot avec couleur
    else if (var1_is_factor && var2_is_numeric) {
      ggplot(hyundai, aes_string(x = input$var_double1, y = "price", color = input$var_double2)) +
        geom_jitter() +  # Scatterplot avec jitter
        scale_color_gradient() +
        labs(title = paste("Distribution du prix en fonction des variables", variable_fr2 , "et", variable_fr3), 
             x = variable_fr2, y = "prix en dollars", color = variable_fr3) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20,  margin = margin(b = 20)),  # Titre en gras et centré
          axis.title.x = element_text(face = "bold", size = 15),  # Nom de l'axe x en gras
          axis.title.y = element_text(face = "bold", size = 15),  # Nom de l'axe y en gras
          axis.text.x = element_text( hjust = 1, angle= 90, face = "bold", size = 12),  # Étiquettes de l'axe x en gras et verticales
          axis.text.y = element_text(face = "bold", size = 12)  # Étiquettes de l'axe y en gras   
        )
    }
  })
  #####
  
  
  observeEvent(input$predict, {
    nouvelles_donnees <- data.frame(
      year = input$year,
      mileage = input$mileage,
      transmission = factor(input$transmission, levels = levels(hyundai$transmission)),
      model = factor(input$model, levels = levels(hyundai$model)),
      fuelType = factor(input$fuelType, levels = levels(hyundai$fuelType)),
      mpg = input$mpg,
      engineSize = input$engineSize
    )
    
    predictions <- predict(modele_prediction, newdata = nouvelles_donnees)
    
    output$result <- renderText({
      if (predictions < 0) {
        return(HTML("<div style='display: flex; justify-content: center; align-items: center; height: 45vh;'>
                    <div style='border: 2px solid red; border-radius: 10px; padding: 20px; text-align: center; background-color: #ffded7; max-width: 600px;'>
                      <h3 style='color: red;'> Malheureusement nous n'avons pas de véhicules correspondants.<br> Essayez d'autres caractéristiques.</h3>
                    </div>
                  </div>"))
      }
      
      HTML(paste("<div style='display: flex; justify-content: center; align-items: center; height: 45vh;'>
                  <div style='border: 2px solid green; border-radius: 10px; padding: 20px; background-color: #cbffd7; text-align: center; max-width: 600px;'>
                    <h3 style='color: green;'> Le prix estimé du véhicule est de", round(predictions, 2), "$</h3>
                  </div>
                </div>"))
    })
  })
  
  


  
}

shinyApp(ui = ui, server = server)

