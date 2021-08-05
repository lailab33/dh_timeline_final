#loading libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(timevis)
library(stringr)
library(rsconnect)


#loading data about the protest, renaming, and deselecting columns that don't matter
data_copy <- read.csv("project_data.csv")

colnames(data_copy) <- c("college", "source", "description", "link", "start", "date_event", 
                         "number_students", "scope_size", "reason", "cause", "tags", "against", 
                         "group_category", "group_name", "effect", "method", "location", "is_linked", 
                         "umbrella_tag", "text_ocr")

#adding a random id to each row, rounding that number and 
data_copy <- data_copy %>%
  mutate(id=sample(1:10000, 53, replace=FALSE))


#converting to numeric for shiny app something
data_copy$id <- as.integer(data_copy$id)


# INPUT SECTION STARTS HERE
ui <- fluidPage(
  
  titlePanel("Vassar and Haverford Protest Timeline"),
  
  br(),
  helpText("This is an interactive timeline of protests recorded by Vassar and Haverford archives,",
           "users can filter what kinds of protests they would like to view on the timeline and",
           "those protests represented are in the table below. If you select a single protest",
           "it will show up under the *Item selected* tab."),
  br(),
  
  # Create a new Row in the UI for selectInputs
  
  fluidRow(
    column(4,
           selectInput("vc.hv",
                       "College:",
                       c("All",
                         unique(as.character(data_copy$college))))
    ),
    column(4,
           selectInput("scp",
                       "Scope:",
                       c("All",
                         unique(as.character(data_copy$scope_size))))
    ),
    column(4,
           selectInput("grp",
                       "Protesting group:",
                       c("All",
                         unique(as.character(data_copy$group_category))))
    )
  ),
  
  fluidRow(
    column(4,
           selectInput("tgs",
                       "Tags:",
                       c("All",
                         unique(as.character(data_copy$tags))))
    ),
    column(4,
           selectInput("agnst",
                       "Against:",
                       c("All",
                         unique(as.character(data_copy$against))))
    ),
    column(4,
           selectInput("mth",
                       "Method:",
                       c("All",
                         unique(as.character(data_copy$method))))
    )
  ),
  
  
  dateRangeInput(
    inputId= "date_range" ,
    label= "Select Date Range to Filter:",
    format = "yyyy-mm-dd", 
    start= as.Date("1970-01-01", format = "%Y-%m-%d"), #IS THIS WHAT ACTUALLY NEEDS TO HAPPEN HERE?
    end= as.Date("2021-01-01", format = "%Y-%m-%d"), #IS THIS WHAT ACTUALLY NEEDS TO HAPPEN HERE?
    startview = "decades",
    separator = " to "
  ),
  
  br(),
  
  # Setting the actual output of the above set up
  titlePanel("Timeline of protests"),
  timevisOutput("timeline"),
  
  br(),
  
#Panel that shows up when you select something in the timeline
  wellPanel(titlePanel("Item Currently Selected"),
                   fluidRow(column(4, wellPanel(h4("Object Source"),
                                                htmlOutput(outputId = "protest_source_info")
                   )
                   ),
                   column(4, wellPanel(h4("Protest Details"),
                                       tableOutput(outputId = "protest_info") 
                   )
                   ),
                   column(4, wellPanel(h4("Object OCR"), 
                                       htmlOutput(outputId = "protest_ocr")
                   )
                   ))),

  br(),
  br(),
  br(),
  
  #Table of protest events reflected in the timeline 

### HOW TO MAKE THIS SMALLER ?? AND NOT HAVE OCR EAT UP SO MUCH SPACE
  titlePanel("Table of events in timeline above"),
  DT::dataTableOutput("table")
)


#------------------------------------------------------------------------------------------------------
##SERVER/OUTPUT SECTION STARTS HERE

#adding the "input$selected" part of the code
server <- function(input, output, session) {
  
  #creating a data table of information variable to inputs of the filter 
    data <- reactive({
      
      #this needs to be here for some reason
      data <- data_copy
      
      #the drop down menu selection reactive functions
      if (input$vc.hv != "All") {
        data <- data[data$college == input$vc.hv,]
      }  
      if (input$grp != "All") {
        data <- data[data$scope_size == input$grp,]
      } 
      if (input$scp != "All") {
        data <- data[data$group_category == input$sc,]
      }
      if (input$agnst != "All") {
      data <- data[data$group_category == input$agnst,]
      }
      if (input$tgs != "All") {
        data <- data[data$tags == input$tgs,]
      }
      if (input$mth != "All") {
        data <- data[data$method == input$mth,]
      }
      
      #date range filter code! which worksssssss!!!!!
      data <- data [which(data$start >= input$date_range[1] & data$start <= input$date_range[2]),]
    
    data })
  
#output to render the table
  output$table <- DT::renderDataTable(DT::datatable(data()))

#output to render the timeline
output$timeline <- renderTimevis({
  timevis(data())
  })


#function that trims white space at the end and at the start
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#creating a reactive search function that matches selected id to dataframe id (for some reason == doesn't work)
timeline_selected_fun <- reactive({
  if (!is.null(input$timeline_selected)) {
    input$timeline_selected %>% 
      str_extract("\\d+") %>% 
      as.integer()
  }
})    

data_selected <- reactive({
  data_copy[timeline_selected_fun() == data_copy$id,]
})


#output of info about the source of the protest
output$protest_source_info <- renderText({
  if (is.null (timeline_selected_fun())) { 
    print("No item selected") } 
  else {
 
     college <-
            paste(strong("College:"),
            print(data_selected()$college))
    
     source <- paste(strong("Source:"),
                     print(data_selected()$source))
     
     description <- paste(strong("Description:"),
                          print(data_selected()$description))
     
     link <- paste(strong("Link:"),
                   print(data_selected()$link))
     
     is_linked <- paste(strong("Is linked to other objects:"),
                        print(data_selected()$is_linked))
     
     umbrella_tag <- paste(strong("Umbrella tag, if linked:"),
                           print(data_selected()$umbrella_tag))
     
     paste(college, source, description, link, is_linked, umbrella_tag, sep = "<br> <br>")
  }
})

#output for the protest details
output$protest_info <- renderText({ 
  if (is.null (timeline_selected_fun())) { 
    print("No item selected") } 
  else {
date_event <- paste(strong("Date of event, if available:"),
                    print(data_selected()$date_event))

number_students <- paste(strong("Number of students:"),
                         print(data_selected()$number_students))

scope_size <- paste(strong("Scope of protest:"),
                    print(data_selected()$scope_size))

reason <- paste(strong("Reason for protest:"),
                print(data_selected()$reason))

cause<- paste(strong("Cause of protest:"),
                     print(data_selected()$cause))

tags<- paste(strong("Tags related to protest:"),
             print(data_selected()$tags))

against <- paste(strong("Protest was against:"),
                 print(data_selected()$against))

group_category <- paste(strong("Kind of group protesting:"),
                        print(data_selected()$group_category))

group_name <- paste(strong("Name of protesting group:"),
                    print(data_selected()$group_name))

effects <- paste(strong("Effect, if known, of protest:"),
                 print(data_selected()$effects))

method <- paste(strong("Method of protest:"),
                print(data_selected()$methods))

location <- paste(strong("Location of protest:"),
                  print(data_selected()$location))

description <- paste(strong("Description:"),
                     print(data_selected()$description))

date_source <- paste(strong("Date of source:"),
                     print(data_selected()$date_source))

paste(date_event, number_students, scope_size, reason, cause, tags, against, 
      group_category, group_name, effects, method, location, description, date_source, sep = "<br> <br>")

}
})

#output of the associated OCR
output$protest_ocr <- renderText({
  if (is.null (timeline_selected_fun())) { 
    print("No item selected") }
    else {
      data.search <- data_copy[timeline_selected_fun() == data_copy$id,]
      print(data.search$text_ocr) }
})
  
  
}

shinyApp(ui = ui, server = server)