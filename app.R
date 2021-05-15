


library(shiny)
library(imputeTS)
library(reactable)
library(dplyr)
library(DT)
library(shinythemes)
library(ggplot2)

setwd("C:/Users/janni/Desktop/gym")

source("mod/utils_new.R")


df = load_df()

ui <- navbarPage("Workout tracker", inverse = TRUE, collapsible = FALSE, position = "static-top",
                 theme = shinytheme("yeti"), 
                 #tags$head(tags$link(rel="shortcut icon", href="C:/Users/janni/Desktop/gym/weightlifting_icon-icons.com_67203.ico")),
                 tabPanel('record workouts and analyse progress'),
  tags$head(
    tags$style(
      HTML(".shiny-notification {
           position:fixed;
           top: calc(20%);
           left: calc(40%);
           }
           "
      )
      )
      ),
  
  # App title ----
  
  sidebarLayout(
    sidebarPanel(width=4,
    div(
      id = "in_form",
      wellPanel(tags$h4("Record new workout"),
                fluidRow(
                  column(width=4,
                         dateInput("date", label = ("Date"), value = Sys.Date(),width = 120 )
                  ),
                  column(width=4,
                         textInput("bw", label="Bodyweight", value = NA, placeholder="in Kg", width=120)
                  )
                    
                  ),
                
        wellPanel(
          tags$p("Exercises, weights and reps"),
          tags$div(id = 'placeholder'),
            fluidRow(actionButton("add_row", "+"),
            actionButton("remove_row","-")
            )
          ),
        
        actionButton("submit", "Submit data", class = "btn-primary")
        )    
      ),
    tags$br(),
    tags$br(),
    wellPanel(
      tags$h4('Choose exercises for analysis'),
      reactableOutput("choice_tabl")
    )
    ),
  
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                      tabPanel("Weights",
                               tags$h3("Weight over time"),
                               plotOutput("wplot", height = 450,
                                          # Equivalent to: click = clickOpts(id = "plot_click")
                                          click = "plot_click",
                                          dblclick = dblclickOpts(
                                            id = "plot_dblclick"
                                          ),
                                          hover = hoverOpts(
                                            id = "plot_hover"
                                          ),
                                          brush = brushOpts(
                                            id = "plot_brush"
                                          )
                          )
                      ),
                  tabPanel("Reps",
                           tags$h3("Repetitions over time"),
                           plotOutput("rplot", height = 450
                           )
                  ),
                  tabPanel("Sets",
                           tags$h3("Sets over time"),
                           plotOutput("splot", height = 450
                           )
                  ),
                  tabPanel("Raw data",
                                    tags$h3("Raw data"),
                           reactableOutput("raw_dat")
                  )
                ),

                fluidRow(
                  column(width=6,
                         sliderInput("rep_slider", label = h3("Rep range"), min = 1, 
                     max = 30, value = c(1, 15))
                    ),
                  column(width=6,
                         dateRangeInput("daterange", label=h3("Date range"),
                            start = min(c(df['date'][[1]])),
                           end   = Sys.Date())
                         )
                  )
                )
      )
   # )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  inserted <- c()
  
  up_dat <- function(){
    
    df <<- load_df()
    plot_dat <<- group_by(df, exercise, date) %>% summarize(weight = mean(weight), reps=mean(reps), sets=mean(sets))
  
    exercises <<- c(plot_dat %>% count(exercise) %>% arrange(desc(n)) %>% select(exercise))[[1]]
    
  }
  
  up_dat()
  
  #updateSelectizeInput(session, NULL, choices = c("",sort(unique(df$exercise))), server = TRUE)
  counter = 0
  observeEvent(input$add_row, {
    
    # id as separate reactive counter, then set plus 1 for input$add_row
    # sothat one row is always shown
    counter = counter + 1
    id = paste0('to_',counter)
    
    insertUI(
      selector = '#placeholder',
      ui = tags$div(
        fluidRow(
          column(width=4,
                 selectizeInput(paste(id,'ex',sep="_"), NULL,
                                c("",exercises), selected=NULL, 
                                options = list(create=TRUE,
                                               placeholder="Exercise"),
                                width="120%")
          ),
          column(width=4,
                 textInput(paste(id,'w',sep="_"), NULL, placeholder = "Weight", width="100%")
          ),
          column(width=4,
                 textInput(paste(id,'r',sep="_"), NULL, placeholder="Reps", width="100%")
          )
        ),
        id=id)
    )
    inserted <<- c(id, inserted)
  })
  
  observeEvent(input$remove_row, {
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', inserted[-length(inserted)])
    )
    inserted <<- inserted[-length(inserted)]
    counter = max(0,counter - 1)
  })
  
  observeEvent(input$submit,{
    
    list_in = list('date' = input$date,
                   'exercises' = list(),
                   'bodyweight' = input$bw)
    
    if(length(inserted)>0){
      for (i in (1:length(inserted))){
        ex = input[[paste0(inserted[i],'_ex')]]
        w = as.integer(input[[paste0(inserted[i],'_w')]])
        r = c(as.numeric(strsplit(input[[paste0(inserted[i],'_r')]],split=",",fixed=TRUE)[[1]]))
        s = length(r)
        
        list_in$exercises[[ex]] = list(
          'weight' = w,
          'reps' = r,
          'sets' = s
        )
      }
      df <<- load_df()
      df <<- add_workout_to_df(list_in=list_in, df=df)
      save_df(df)
      for (i in (1:length(inserted))){
        removeUI(
          selector = paste0('#', inserted[i]))
        counter = 0
      }
      inserted<- c()
      
      if(list_in$bodyweight!=''){
        bw_data = load_bw_data()
        df_aux = tibble(date=list_in$date, bodyweight=list_in$bodyweight)
        bw_data = bind_rows(bw_data, df_aux)
        save_bw_df(bw_data)
      }
      
      #up_dat()
      showNotification('Added workout to dataframe!', type="message",duration=2)
    }
  })
  
  
  
  output$choice_tabl <- renderReactable({
    reactable(data.frame(exercises),
              columns=list(exercises = colDef(width=200, name="Exercises")),
              pagination = TRUE,
              sortable = TRUE,
              searchable = TRUE,
              highlight=TRUE,
              #resizeable=TRUE,
              selection="multiple",
              onClick="select",
              defaultSelected = c(1:5), #match(c("bench_press", "squat", "press", "deadlift", "power_clean"),exercises),#,
              defaultSortOrder = "asc",
              bordered=TRUE
    )
  })
  
  output$raw_dat <- renderReactable({
    reactable(df,
              pagination = TRUE,
              defaultPageSize = 50,
              sortable = TRUE,
              searchable = TRUE,
              highlight=TRUE,
              #resizeable=TRUE,
              defaultSortOrder = "asc",
              bordered=TRUE
    )
  })
  


  rep_range <- reactive({
    cbind(input$rep_slider[1],input$rep_slider[2])
  })
  
  d_range <- reactive({
    cbind(input$daterange[1],input$daterange[2])
  })
  
  output$table_state <- renderPrint({
    #a <- req(input$rep_slider[1])
    print({d_range()})
  })

  
  sub_plot_dat = reactive({plot_dat %>%
      filter(exercise %in%
      exercises[req(getReactableState("choice_tabl", name="selected"))]) %>%
      filter(between(reps,req(rep_range()[1]),req(rep_range()[2]))) %>%
      filter(between(date,req(d_range()[1]),req(d_range()[2])))
    })
  
  output$wplot <- renderPlot({
    ggplot(sub_plot_dat(), aes(date,weight)) + geom_point(aes(col=exercise), size=4) + geom_line(aes(col=exercise)) + 
      labs(y="Weight in kg", x="Date") + theme(
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 20)
      )
  })  

  output$rplot <- renderPlot({
      ggplot(sub_plot_dat(), aes(date,reps)) + geom_point(aes(col=exercise), size=4) + geom_line(aes(col=exercise)) + 
      labs(y="(Average) repetitions per set", x="Date", fill="Exercise")+ theme(
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 20)
      )
  })
  
  output$splot <- renderPlot({
    ggplot(sub_plot_dat(), aes(date,sets)) + geom_point(aes(col=exercise), size=4) + geom_line(aes(col=exercise)) + 
      labs(y="Sets", x="Date", fill="Exercise")+ theme(
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 20)
      )
  })
  
  
}

options(shiny.port = 9999)
shinyApp(ui, server)



# histograms of rep ranges
# hover over for plots
# LR for weight / reps progress 




