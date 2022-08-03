library(shiny)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(data.table)
library(DT)
library(rstatix)
library(jsmodule)

ui <- fluidPage(
  titlePanel(NULL),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file", label = "Upload file here", accept = ".csv"),
      selectInput(inputId = "select", label = NULL, choices = NULL),
      selectInput(inputId = "select2", label = NULL, choices = NULL),
      checkboxInput(inputId = "view_p", label = "View P-value"),
      selectizeInput(inputId = "strata", label = "Strata", choices = NULL)
    ),
    mainPanel(
      plotOutput(outputId = "disPlot"),
    )
  )
)

checkFactor <- function(i, count = 10){
  return(length(unique(i)) < count)
}

server <- function(input, output, session) {
  
  userData <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    file <- input$file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    userData (read.csv(file$datapath))
    
    cols <- which(sapply(userData(), checkFactor))
    
    userData(
      userData() %>% mutate_at(cols, factor)
    )
    
    output$table <- DT::renderDataTable(datatable(userData()))
    
    updateSelectInput(
      inputId = "select", 
      label = "X variable", 
      choices = colnames(userData()), 
      selected = NULL
    )
    
    updateSelectInput(
      inputId = "select2", 
      label = "Y variable", 
      choices = colnames(userData()), 
      selected = NULL
    )
    
  })
  
  observeEvent(list(input$select, input$select2), {  
    req(input$select) 
    req(input$select2)
    if(input$select == input$select2){return()}
    
    cols <- names(which(sapply(userData(), checkFactor))) 
    strata_vars <- setdiff(cols, c(input$select, input$select2))
    strata_select <- c("None", strata_vars)
    
    updateSelectizeInput(
      inputId = "strata", 
      label = "Strata", 
      choices = strata_select, 
      selected = "None"
    )
    
  })
  
  observeEvent(input$strata, {
    req(input$strata)  
    
    output$disPlot <- renderPlot({
      
      stat.test <- userData() %>%
        t_test(as.formula(paste0(input$select2, " ~ ", input$select))) %>%
        add_significance()
      stat.test <- stat.test %>% add_xy_position(x = input$select, dodge = 0.8)
      
      if (input$strata == "None") {
        Plot <- ggboxplot(
          userData(), 
          x = input$select, 
          y = input$select2
        )
      } else {
        Plot <- ggboxplot(
          userData(),
          x = input$select, 
          y = input$select2, 
          color = input$strata
        )
      }
      if (input$view_p) {
        Plot <- Plot + stat_pvalue_manual(stat.test, label = "p={p}", vjust = -1, bracket.nudge.y = 1) +
          scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))
      }
      return(Plot)
    })
  })
  
}
shinyApp(ui, server)
