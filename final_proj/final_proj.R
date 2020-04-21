#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rvest)
library(gt)
library(janitor)
library(ggplot2)
library(tidytext)
library(glue)
library(stringr)
library(plotly)

files <- list.files("input")

readfile <- function(file){
    
    # get the file
    
    fileName <- glue("input/", file, sep = "")
    
    # get rid of any trailing spaces
    
    fileName <- trimws(fileName)
    
    # read in the new file
    
    fileText <- glue(read_file(fileName))
    
    # remove any dollar signs (they're special characters in R)
    
    fileText <- gsub("\\$", "", fileText)
    
    # create the tibble using the file's text, file name, year by grabbing the last four digits of file name, and president
    
    base <- tibble(text = fileText, file = file, year = as.numeric(str_match(file, "\\d{4}")), president = str_match(file, "(.*?)_")[2])
    
    # return that tibble
    
    return(base)
}

# create a blank tibble

base_file <- tibble()

# run through all files in input

for(i in files){
    
    # found out about Rbind on stackoverflow, it was applicable here
    
    base_file <- rbind(base_file, readfile(i))
}

base_file_gt <- base_file %>%
    mutate(speech_text = paste(str_sub(text, 1, 1000), "...")) %>%
    sample_n(size = 5) %>%
    select(year, president, text) %>%
    gt() %>%
    cols_label(year = "Year", president = "President", speech_text = "Text")

ui <- navbarPage(
    "State of the Union Text Analysis",
    tabPanel("Introduction",
             titlePanel("Introduction"), p("Welcome to my final project for Gov 1005: Data!"),
             p("Article 2, Section 3 of the Constitution describes that the President shall 'from time to time give to the Congress Information of the State of the Union, and recommend to their Consideration such measures as he shall judge necessary and expedient.'"),
             p("This one of the few political traditions that every President from Washington to Trump has partaken in. For two centuries, every President has taken the chance to report an assessment on the condition of the United States, recommend a legislative program for the coming session of Congress, and present a vision for the future."),
             p("1,773,287 total words have been spoken in this tradition, creating a fascinating trove of data. This project seeks to apply a series of data science techniques to better understand the State of the Union, including sentiment analysis of the positivity and tone of each address, topic modeling to illustrate the relationship of words and ideas to one another, and machine learning in the form of natural language generation to create a 'new', automated State of the Union address.")),
    tabPanel("Data",
             titlePanel("Data"),
             p("The data used in this project is from this corpus on Kaggle:"),
             p("https://www.kaggle.com/jyronw/us-state-of-the-union-addresses-1790-2019"),
             p("I initially combined this data with other information I found on State of the Union addresses, but this proved frivolous to my ultimate analysis. I decided to derive most of my insights from the text data alone absent other factors."),
             p("Here is a random sample of the beginnings of five speeches in my data:"),
             ),
    tabPanel("Sentiment Analysis",
             titlePanel("Discussion Title"),
             p("DATA")),
    tabPanel("Topic Modeling",
             titlePanel("Discussion Title"),
             p("DATA")),
    tabPanel("Natural Language Generation",
             titlePanel("Discussion Title"),
             p("DATA")),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("DATA")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")))

server <- function(input, output) {
    output$line_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        ifelse(
            input$plot_type == "a",
            
            # If input$plot_type is "a", plot histogram of "waiting" column 
            # from the faithful dataframe
            
            x   <- faithful[, 2],
            
            # If input$plot_type is "b", plot histogram of "eruptions" column
            # from the faithful dataframe
            
            x   <- faithful[, 1]
        )
        
        # Draw the histogram with the specified number of bins
        
        hist(x, col = 'darkgray', border = 'white')
    })
}

shinyApp(ui = ui, server = server)

