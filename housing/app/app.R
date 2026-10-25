# ============================================================
# Application Shiny
# Ridge / Lasso - Ames Housing
# ============================================================

library(shiny)
library(glmnet)
library(caret)
library(ggplot2)

# ------------------------------------------------------------
# Données
# ------------------------------------------------------------

data <- read.csv(
  "../data/AmesHousing.csv",
  stringsAsFactors = FALSE
)

# Variables simples pour la démonstration Shiny
app_data <- data %>%
  select(
    saleprice,
    overall_qual,
    gr_liv_area,
    year_built,
    total_bsmt_sf,
    garage_cars,
    full_bath,
    bedroom_abvgr
  ) %>%
  na.omit()

# ------------------------------------------------------------
# Préparation
# ------------------------------------------------------------

x <- model.matrix(
  saleprice ~ .,
  app_data
)[, -1]

y <- app_data$saleprice

set.seed(123)

index <- createDataPartition(
  y,
  p = 0.8,
  list = FALSE
)

x_train_app <- x[index, ]
x_test_app <- x[-index, ]

y_train_app <- y[index]
y_test_app <- y[-index]

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel(
    "Prédiction du prix immobilier - Ridge & Lasso"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "model",
        "Choisir le modèle :",
        choices = c(
          "Ridge" = 0,
          "Lasso" = 1
        )
      ),
      
      sliderInput(
        "quality",
        "Qualité générale :",
        min = 1,
        max = 10,
        value = 7
      ),
      
      numericInput(
        "area",
        "Surface habitable (sq ft) :",
        value = 1500,
        min = 300
      ),
      
      numericInput(
        "year",
        "Année de construction :",
        value = 2000,
        min = 1800,
        max = 2020
      ),
      
      numericInput(
        "basement",
        "Surface du sous-sol :",
        value = 1000,
        min = 0
      ),
      
      sliderInput(
        "garage",
        "Nombre de voitures dans le garage :",
        min = 0,
        max = 5,
        value = 2
      ),
      
      sliderInput(
        "bath",
        "Nombre de salles de bain :",
        min = 0,
        max = 5,
        value = 2
      ),
      
      sliderInput(
        "bedrooms",
        "Nombre de chambres :",
        min = 0,
        max = 8,
        value = 3
      ),
      
      actionButton(
        "predict",
        "Prédire le prix"
      )
    ),
    
    mainPanel(
      
      h3("Prix estimé"),
      
      verbatimTextOutput(
        "prediction"
      ),
      
      hr(),
      
      h3("Performance du modèle"),
      
      tableOutput(
        "performance"
      ),
      
      plotOutput(
        "predictionPlot"
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------

server <- function(input, output) {
  
  model_results <- reactive({
    
    alpha_value <- as.numeric(input$model)
    
    set.seed(123)
    
    cv_model <- cv.glmnet(
      x_train_app,
      y_train_app,
      alpha = alpha_value,
      nfolds = 10
    )
    
    lambda_best <- cv_model$lambda.min
    
    model <- glmnet(
      x_train_app,
      y_train_app,
      alpha = alpha_value,
      lambda = lambda_best
    )
    
    predictions <- predict(
      model,
      newx = x_test_app
    )
    
    list(
      model = model,
      lambda = lambda_best,
      predictions = as.numeric(predictions)
    )
  })
  
  # ----------------------------------------------------------
  # Performance
  # ----------------------------------------------------------
  
  output$performance <- renderTable({
    
    res <- model_results()
    
    predictions <- res$predictions
    
    data.frame(
      Mesure = c(
        "Lambda optimal",
        "RMSE",
        "MAE",
        "R²"
      ),
      
      Valeur = c(
        round(res$lambda, 4),
        
        round(
          RMSE(predictions, y_test_app),
          2
        ),
        
        round(
          MAE(predictions, y_test_app),
          2
        ),
        
        round(
          R2(predictions, y_test_app),
          4
        )
      )
    )
  })
  
  # ----------------------------------------------------------
  # Nouvelle maison
  # ----------------------------------------------------------
  
  new_house <- eventReactive(
    input$predict,
    {
      
      matrix(
        c(
          input$quality,
          input$area,
          input$year,
          input$basement,
          input$garage,
          input$bath,
          input$bedrooms
        ),
        nrow = 1,
        dimnames = list(
          NULL,
          colnames(x_train_app)
        )
      )
    }
  )
  
  # ----------------------------------------------------------
  # Prédiction
  # ----------------------------------------------------------
  
  output$prediction <- renderText({
    
    req(new_house())
    
    res <- model_results()
    
    pred <- predict(
      res$model,
      newx = new_house()
    )
    
    paste0(
      "Prix estimé : $",
      format(
        round(as.numeric(pred), 0),
        big.mark = ","
      )
    )
  })
  
  # ----------------------------------------------------------
  # Graphique réel / prédit
  # ----------------------------------------------------------
  
  output$predictionPlot <- renderPlot({
    
    res <- model_results()
    
    plot_data <- data.frame(
      reel = y_test_app,
      predit = res$predictions
    )
    
    ggplot(
      plot_data,
      aes(x = reel, y = predit)
    ) +
      geom_point(alpha = 0.5) +
      geom_abline(
        slope = 1,
        intercept = 0,
        linetype = "dashed"
      ) +
      labs(
        title = "Prix réels vs prix prédits",
        x = "Prix réel ($)",
        y = "Prix prédit ($)"
      ) +
      theme_minimal()
  })
}

shinyApp(
  ui = ui,
  server = server
)