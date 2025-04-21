library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stats)

ui <- fluidPage(
  titlePanel("Análisis estadístico y gráfico de datos"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sube un archivo CSV", accept = ".csv"),
      uiOutput("var_select1"),
      uiOutput("var_select2")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Datos", tableOutput("tabla")),
        tabPanel("Prueba estadística", verbatimTextOutput("prueba")),
        tabPanel("Gráfico", plotOutput("grafico"))
      )
    )
  )
)

server <- function(input, output, session) {
  datos <- reactive({
    req(input$file)
    read_csv(input$file$datapath, show_col_types = FALSE)
  })
  
  output$tabla <- renderTable({
    datos()
  })
  
  output$var_select1 <- renderUI({
    req(datos())
    selectInput("var1", "Selecciona variable 1", choices = names(datos()))
  })
  
  output$var_select2 <- renderUI({
    req(datos())
    selectInput("var2", "Selecciona variable 2", choices = names(datos()))
  })
  
  tipo_variable <- function(v) {
    if (is.numeric(v)) "cuantitativa" else "cualitativa"
  }
  
  output$prueba <- renderPrint({
    req(input$var1, input$var2)
    df <- datos()
    v1 <- df[[input$var1]]
    v2 <- df[[input$var2]]
    tipo1 <- tipo_variable(v1)
    tipo2 <- tipo_variable(v2)
    
    if (tipo1 == "cuantitativa" && tipo2 == "cuantitativa") {
      cat("Correlación:\n")
      pearson <- cor.test(v1, v2, method = "pearson")
      spearman <- cor.test(v1, v2, method = "spearman")
      print(pearson)
      cat("\n")
      print(spearman)
    } else if (tipo1 == "cualitativa" && tipo2 == "cualitativa") {
      tab <- table(v1, v2)
      if (all(dim(tab) == 2)) {
        cat("Chi-cuadrado:\n")
        print(chisq.test(tab))
        if (all(tab >= 10)) {
          cat("\nMcNemar:\n")
          print(mcnemar.test(tab))
        }
        cat("\nCochrane (test de Cochran no implementado directamente en R base, se necesita librería adicional).")
      } else {
        cat("Chi-cuadrado:\n")
        print(chisq.test(tab))
      }
    } else {
      num <- if (tipo1 == "cuantitativa") v1 else v2
      grupo <- if (tipo1 == "cuantitativa") v2 else v1
      grupo <- as.factor(grupo)
      
      if (nlevels(grupo) == 2) {
        cat("Prueba F de Student:\n")
        print(t.test(num ~ grupo))
        cat("\nPrueba de Wilcoxon:\n")
        print(wilcox.test(num ~ grupo))
      } else if (nlevels(grupo) > 2) {
        cat("ANOVA:\n")
        print(summary(aov(num ~ grupo)))
      } else {
        cat("No se puede realizar prueba con solo un nivel en el grupo.")
      }
    }
  })
  
  output$grafico <- renderPlot({
    req(input$var1, input$var2)
    df <- datos()
    v1 <- df[[input$var1]]
    v2 <- df[[input$var2]]
    tipo1 <- tipo_variable(v1)
    tipo2 <- tipo_variable(v2)
    
    if (tipo1 == "cuantitativa" && tipo2 == "cuantitativa") {
      ggplot(df, aes(x = !!sym(input$var1), y = !!sym(input$var2))) +
        geom_point() + geom_smooth(method = "lm", se = FALSE) +
        theme_minimal()
    } else if (tipo1 == "cualitativa" && tipo2 == "cualitativa") {
      ggplot(df, aes(x = !!sym(input$var1), fill = !!sym(input$var2))) +
        geom_bar(position = "dodge") +
        theme_minimal()
    } else {
      ggplot(df, aes(x = !!sym(input$var2), y = !!sym(input$var1))) +
        geom_boxplot() +
        theme_minimal()
    }
  })
}

shinyApp(ui, server)