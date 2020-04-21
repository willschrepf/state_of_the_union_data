library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(glue)
library(tidytext)
library(stringr)
library(rvest)
library(gt)

files <- list.files("input")

GetSentiment <- function(file){
    
    # beginning of this file is exact same as earlier readfile function
    
    fileName <- glue("input/", file, sep = "")
    fileName <- trimws(fileName)
    fileText <- glue(read_file(fileName))
    fileText <- gsub("\\$", "", fileText)
    
    # break down text into individual words
    
    tokens <- tibble(text = fileText) %>% unnest_tokens(word, text)
    
    # get the total number of words
    
    num_words <- tokens %>%
        count()
    
    # get the sentiment from the text:
    
    sentiment <- tokens %>%
        
        # uses the existing Bing sentiment lexicon, more on this in the Kaggle link
        
        inner_join(get_sentiments("bing")) %>%
        
        # count the number of positive & negative words
        
        count(sentiment) %>%
        
        # make data wide rather than narrow
        
        spread(sentiment, n, fill = 0) %>%
        
        # number of positive words minus number of negative words divided by total words
        
        mutate(sentiment = ((positive - negative)/as.double(num_words))) %>%
        
        # add file name, year, and president like I did before
        
        mutate(file = file) %>%
        mutate(year = as.numeric(str_match(file, "\\d{4}"))) %>%
        mutate(president = str_match(file, "(.*?)_")[2])
    
    # return our sentiment tibble
    
    return(sentiment)
}

# exactly as before, make a blank tibble and read in results with rbind

sentiments <- tibble()

for(i in files){
    sentiments <- rbind(sentiments, GetSentiment(i))
}


# disambiguate Bush Sr. and George W. Bush
# correct president in applicable rows

bushSr <- sentiments %>%
    filter(president == "Bush") %>%
    filter(year < 2000) %>%
    mutate(president = "Bush Sr.")

# remove incorrect rows

sentiments <- anti_join(sentiments, sentiments[sentiments$president == "Bush" & sentiments$year < 2000, ])

# add corrected rows to data_frame

sentiments <- full_join(sentiments, bushSr)

# same process but for John Adams and John Quincy Adams

quincyAdams <- sentiments %>%
    filter(president == "Adams") %>%
    filter(year > 1824) %>%
    mutate(president = "Quincy Adams")

sentiments <- anti_join(sentiments, sentiments[sentiments$president == "Adams" & sentiments$year > 1824, ])

sentiments <- full_join(sentiments, quincyAdams)

# same process but for FDR and Teddy Roosevelt

FDR <- sentiments %>%
    filter(president == "Roosevelt") %>%
    filter(year > 1932) %>%
    mutate(president = "F. Roosevelt")

sentiments <- anti_join(sentiments, sentiments[sentiments$president == "Roosevelt" & sentiments$year > 1932, ])

sentiments <- full_join(sentiments, FDR)

sentiments <- sentiments %>%
    mutate(president = ifelse(president == "Roosevelt", "T. Roosevelt", president))

# filter for only speeches from the last 100 years (expanded slightly to include all of Wilson's term)

sentiments <- sentiments %>%
    filter(year >= 1913)

# manually add in the party of each president
# I could have found an online table, but that would honestly
# have taken longer than manually inputting this minute amount of data

sentiments <- sentiments %>%
    mutate(Party = "party") %>%
    mutate(Party = ifelse(president == "Wilson", "Democrat", Party)) %>%
    mutate(Party = ifelse(president == "Harding", "Republican", Party)) %>%
    mutate(Party = ifelse(president == "Coolidge", "Republican", Party)) %>%
    mutate(Party = ifelse(president == "Hoover", "Republican", Party)) %>%
    mutate(Party = ifelse(president == "F. Roosevelt", "Democrat", Party)) %>%
    mutate(Party = ifelse(president == "Truman", "Democrat", Party)) %>%
    mutate(Party = ifelse(president == "Eisenhower", "Republican", Party)) %>%
    mutate(Party = ifelse(president == "Kennedy", "Democrat", Party)) %>%
    mutate(Party = ifelse(president == "Johnson", "Democrat", Party)) %>%
    mutate(Party = ifelse(president == "Nixon", "Republican", Party)) %>%
    mutate(Party = ifelse(president == "Ford", "Republican", Party)) %>%
    mutate(Party = ifelse(president == "Carter", "Democrat", Party)) %>%
    mutate(Party = ifelse(president == "Reagan", "Republican", Party)) %>%
    mutate(Party = ifelse(president == "Bush Sr.", "Republican", Party)) %>%
    mutate(Party = ifelse(president == "Clinton", "Democrat", Party)) %>%
    mutate(Party = ifelse(president == "Bush", "Republican", Party)) %>%
    mutate(Party = ifelse(president == "Obama", "Democrat", Party)) %>%
    mutate(Party = ifelse(president == "Trump", "Republican", Party)) %>%
    
    # turn year into numeric
    
    mutate(year = as.numeric(year))


base_file_gt <- base_file %>%
    mutate(speech_text = paste(str_sub(text, 1, 1000), "...")) %>%
    sample_n(size = 5) %>%
    select(year, president, speech_text) %>%
    gt() %>%
    cols_label(year = "Year", president = "President", speech_text = "Text")


shinyApp(
    ui = navbarPage(
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
                 gt_output(outputId = "base_file_gt")),
        tabPanel("Sentiment Analysis",
                 titlePanel("Sentiment Analysis"),
                 p("A sentiment analysis quantifies the positive and negative tone inherent in a speech. Using a comprehensive lexicon of words, I calculated the average positive sentiment of every State of the Union was calculated. In order to maintain the lexicon's viability, I only used 'modern' speeches- those delivered in a spoken format in the last hundred years (expanded slightly to include all of Pres. Wilson's speeches)- so that the lexicon did not struggle to analyze the diction and speech patterns of 19th and 18th century English."),
                 p("Hover over indiviudal data points to see more specifics! There are several interesting historical trends that can be seen here- Dwight Eisenhower tended to be the most positive speaker, and FDR's addresses during the Depression and Second World War were the only speeches with a net negativity."),
                 plotlyOutput("sentiment_plot")),
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
             You can reach me at ______@college.harvard.edu."))),
    server = function(input, output) {
        output$base_file_gt <- render_gt(
            expr = base_file_gt,
            height = px(600),
            width = px(600)
        )
        
        output$sentiment_plot <- renderPlotly({
            print(
            ggplotly(ggplot(sentiments, aes(x = year, y = sentiment)) +
                         labs(x = "Year", y = "Positive Sentiment", title = "Positive Sentiment of Modern State of the Union Addresses") +
                         geom_point(aes(color = Party, president = president)) +
                         
                         # add colors by party, not by president
                         scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
                         
                         # manually input vertical lines to disambiguate between presidents
                         
                         geom_vline(xintercept = 1920.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 1922.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 1928.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 1932.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 1945.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 1953.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 1961.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 1963.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 1969.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 1974.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 1977.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 1981.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 1988.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 1992.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 2000.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 2008.5, linetype="dotted", color = "darkgrey", size= .4) +
                         geom_vline(xintercept = 2016.5, linetype="dotted", color = "darkgrey", size= .4), tooltip = c("president", "year", "sentiment")))}, 
        )
    }
)

