library(shiny)
library(httr)
library(jsonlite)
library(plotly)
library(bslib)

API_BASE <- Sys.getenv("API_BASE", unset = "http://api:8000")
API_URL  <- paste0(API_BASE, "/predict")

ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = c("Arial", "sans-serif")
  ),
  title = "Diabetes Risk Prediction",
  sidebar = sidebar(
    h4("Patient profile"),
    h5("Demographics"),
    sliderInput("Age", "Age group (BRFSS category)", min = 1, max = 13, value = 7, step = 1),
    sliderInput("Sex", "Sex (0 = female, 1 = male)", min = 0, max = 1, value = 0, step = 1),
    sliderInput("Education", "Education level", min = 1, max = 6, value = 4, step = 1),
    sliderInput("Income", "Income level", min = 1, max = 8, value = 5, step = 1),
    hr(),
    h5("Lifestyle"),
    sliderInput("Smoker", "Current smoker (0/1)", min = 0, max = 1, value = 0, step = 1),
    sliderInput("PhysActivity", "Physical activity (0/1)", min = 0, max = 1, value = 1, step = 1),
    sliderInput("Fruits", "Consumes fruits regularly (0/1)", min = 0, max = 1, value = 1, step = 1),
    sliderInput("Veggies", "Consumes vegetables regularly (0/1)", min = 0, max = 1, value = 1, step = 1),
    sliderInput("HvyAlcoholConsump", "Heavy alcohol consumption (0/1)", min = 0, max = 1, value = 0, step = 1),
    hr(),
    h5("Clinical / Health status"),
    sliderInput("HighBP", "High blood pressure (0/1)", min = 0, max = 1, value = 0, step = 1),
    sliderInput("HighChol", "High cholesterol (0/1)", min = 0, max = 1, value = 0, step = 1),
    sliderInput("CholCheck", "Cholesterol check in last 5 years (0/1)", min = 0, max = 1, value = 1, step = 1),
    numericInput("BMI", "BMI", value = 27.3, min = 10, max = 60, step = 0.1),
    sliderInput("Stroke", "Ever had stroke (0/1)", min = 0, max = 1, value = 0, step = 1),
    sliderInput("HeartDiseaseorAttack", "Coronary heart disease / MI (0/1)", min = 0, max = 1, value = 0, step = 1),
    sliderInput("AnyHealthcare", "Any healthcare coverage (0/1)", min = 0, max = 1, value = 1, step = 1),
    sliderInput("NoDocbcCost", "Could not see doctor because of cost (0/1)", min = 0, max = 1, value = 0, step = 1),
    sliderInput("DiffWalk", "Difficulty walking (0/1)", min = 0, max = 1, value = 0, step = 1),
    sliderInput("GenHlth", "General health (1=excellent, 5=poor)", min = 1, max = 5, value = 3, step = 1),
    sliderInput("MentHlth", "Bad mental health days (0–30)", min = 0, max = 30, value = 0, step = 1),
    sliderInput("PhysHlth", "Bad physical health days (0–30)", min = 0, max = 30, value = 0, step = 1),
    hr(),
    actionButton("go", "Compute risk", class = "btn-primary", width = "100%")
  ),
  
  fluidPage(
    fluidRow(
      column(
        width = 4,
        card(
          card_header("Final class"),
          card_body(
            uiOutput("final_class_badge")
          )
        )
      ),
      column(
        width = 4,
        card(
          card_header("Diabetes probability"),
          card_body(
            uiOutput("p_diabetes_value"),
            uiOutput("p_diabetes_bar")
          )
        )
      ),
      column(
        width = 4,
        card(
          card_header("API status"),
          card_body(
            textOutput("status_text")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        card(
          card_header("Probabilities (pie chart)"),
          plotlyOutput("pie_chart", height = "380px")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        card(
          card_header("Probabilities (table)"),
          tableOutput("probs")
        )
      ),
      column(
        width = 6,
        card(
          card_header("Classes"),
          tableOutput("classes")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  call_api <- eventReactive(input$go, {
    payload <- list(
      HighBP = input$HighBP,
      HighChol = input$HighChol,
      CholCheck = input$CholCheck,
      BMI = input$BMI,
      Smoker = input$Smoker,
      Stroke = input$Stroke,
      HeartDiseaseorAttack = input$HeartDiseaseorAttack,
      PhysActivity = input$PhysActivity,
      Fruits = input$Fruits,
      Veggies = input$Veggies,
      HvyAlcoholConsump = input$HvyAlcoholConsump,
      AnyHealthcare = input$AnyHealthcare,
      NoDocbcCost = input$NoDocbcCost,
      GenHlth = input$GenHlth,
      MentHlth = input$MentHlth,
      PhysHlth = input$PhysHlth,
      DiffWalk = input$DiffWalk,
      Sex = input$Sex,
      Age = input$Age,
      Education = input$Education,
      Income = input$Income
    )
    
    resp <- try(
      POST(
        url = API_URL,
        body = payload,
        encode = "json"
      ),
      silent = TRUE
    )
    
    if (inherits(resp, "try-error")) {
      return(list(
        error = TRUE,
        status = NA_integer_,
        content = NULL
      ))
    }
    
    status <- status_code(resp)
    content_txt <- content(resp, as = "text", encoding = "UTF-8")
    content <- tryCatch(fromJSON(content_txt), error = function(e) NULL)
    
    list(
      error = FALSE,
      status = status,
      content = content
    )
  })
  
  output$status_text <- renderText({
    res <- call_api()
    if (isTRUE(res$error) || is.null(res$content)) {
      return("Error: could not reach API or invalid response.")
    }
    paste0("HTTP status: ", res$status)
  })
  
  output$final_class_badge <- renderUI({
    res <- call_api()
    if (isTRUE(res$error) || is.null(res$content$classes)) {
      return(tags$span("N/A", class = "badge bg-secondary"))
    }
    cls <- res$content$classes$final_class
    col <- switch(
      cls,
      "diabetes" = "bg-danger",
      "prediabetes" = "bg-warning",
      "healthy" = "bg-success",
      "bg-secondary"
    )
    tags$span(
      toupper(cls),
      class = paste("badge", col),
      style = "font-size: 1.2rem; padding: 0.6rem 1rem;"
    )
  })
  
  output$p_diabetes_value <- renderUI({
    res <- call_api()
    if (isTRUE(res$error) || is.null(res$content$probabilities)) {
      return(tags$p("p_diabetes: N/A"))
    }
    p <- res$content$probabilities$p_diabetes
    tags$p(
      paste0("p_diabetes: ", sprintf("%.1f%%", 100 * p)),
      style = "font-size: 1.1rem; font-weight: 600;"
    )
  })
  
  output$p_diabetes_bar <- renderUI({
    res <- call_api()
    if (isTRUE(res$error) || is.null(res$content$probabilities)) return(NULL)
    p <- res$content$probabilities$p_diabetes
    width_pct <- max(2, min(100, 100 * p))
    col <- if (p < 0.2) {
      "#4CAF50"
    } else if (p < 0.5) {
      "#FFC107"
    } else {
      "#F44336"
    }
    tags$div(
      style = "background-color:#eee; border-radius:4px; height:20px; width:100%;",
      tags$div(
        style = paste0(
          "height:100%; width:", sprintf("%.1f", width_pct), "%;",
          "background-color:", col, "; border-radius:4px;"
        )
      )
    )
  })
  
  output$pie_chart <- renderPlotly({
    res <- call_api()
    if (isTRUE(res$error) || is.null(res$content$probabilities)) return(NULL)
    
    probs <- res$content$probabilities
    df_pie <- data.frame(
      category = c("Diabetes", "Prediabetes", "Healthy"),
      prob = c(probs$p_diabetes, probs$p_prediabetes, probs$p_healthy),
      color = c("#FF6B6B", "#FFA726", "#66BB6A")
    )
    
    plot_ly(
      df_pie,
      labels = ~category,
      values = ~prob,
      type = "pie",
      textposition = "inside",
      textinfo = "label+percent",
      marker = list(colors = df_pie$color),
      hovertemplate = "%{label}: %{value:.1%}<extra></extra>"
    )
  })
  
  output$probs <- renderTable({
    res <- call_api()
    if (isTRUE(res$error) || is.null(res$content$probabilities)) return(NULL)
    p <- res$content$probabilities
    data.frame(
      Probability = names(p),
      Value = sprintf("%.4f", unlist(p))
    )
  }, striped = TRUE, bordered = TRUE)
  
  output$classes <- renderTable({
    res <- call_api()
    if (isTRUE(res$error) || is.null(res$content$classes)) return(NULL)
    c <- res$content$classes
    data.frame(
      Class = names(c),
      Value = unlist(c)
    )
  }, striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
