


library(shiny)
library(imputeTS)
library(reactable)
library(dplyr)
library(DT)

setwd("C:/Users/janni/Desktop/gym")

source("mod/utils_new.R")


df = load_df()

#tags$div(id = 'placeholder')

ui <- fluidPage(
  
  # App title ----
  titlePanel("Workout progress tracker"),
  
  sidebarLayout(
    sidebarPanel(width=4,
                 div(
                   id = "in_form",
                   tags$h1("Add workout"),
                   dateInput("date", label = ("Date"), value = Sys.Date(),width = 100 ),
                   
                   wellPanel(uiOutput("interactionUI"),
                             actionButton("add_row", "+")),
                   
                   
                   #tags$br(),
                   #tags$br(),
                   #tags$br(),
                   
                   actionButton("submit", "Submit Workout", class = "btn-primary")    
                   #checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
                   #sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
                 )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel("main")
    # Output: Histogram ----
  )
  
)


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  
  counter <- reactiveValues(n = 1)
  
  observeEvent(input$add_row,
               {
                 counter$n <- counter$n + 1
               }
  )
  
  
  
  output$interactionUI <- renderUI({
    
    n <- counter$n
    
    if (n >= 1) {
      interaction <- lapply(seq_len(n), function(i) {
        
        fluidRow(
          column(width=4, offset=0,
                 selectInput(paste0("exercise", i), "Exercise",
                             c(unique(df$exercise)), width="100%")
          ),
          column(width=4,
                 textInput(paste0("weight", i), "Weight", width="60%")
          ),
          column(width=4,
                 textInput(paste0("reps", i), "Reps", width="80%")
          )
        )
        
      })
    }
    do.call(flowLayout, interaction) 
  })
}


shinyApp(ui, server)