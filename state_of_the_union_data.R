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
        
        # uses the existing Bing sentiment lexicon
        
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


shinyApp(
    ui = navbarPage(
        "State of the Union Text Analysis",
        tabPanel("About",
                 titlePanel("Introduction"), 
                 h3("Welcome to my final project for Gov 1005: Data!"),
                 
                 # found out that images can be easily accessed from folder www/ (working directory)
                 
                 img(src = "SOTU.jpg", align = "center"),
                 p(""),
                 p("Article 2, Section 3 of the Constitution describes that the President shall 'from time to time give to the Congress Information of the State of the Union, and recommend to their Consideration such measures as he shall judge necessary and expedient.'"),
                 p("This one of the few political traditions that every President from Washington to Trump has partaken in. For two centuries, every President has taken the chance to report an assessment on the condition of the United States, recommend a legislative program for the coming session of Congress, and present a vision for the future."),
                 p("1,773,287 total words have been spoken in this tradition, creating a fascinating trove of data. This project seeks to apply a series of data science techniques to better understand the State of the Union, including sentiment analysis of the positivity and tone of each address, topic modeling to illustrate the relationship of words and ideas to one another, and machine learning in the form of natural language generation to create a 'new', automated State of the Union address."),
                 a(href = "https://www.kaggle.com/jyronw/us-state-of-the-union-addresses-1790-2019", "The data used in this project is from this corpus on Kaggle."),
                 p(""),
                 p("I initially combined this data with other information I found on State of the Union addresses (you can still see that in my code, so I believe I met the project requirement guidelines), but this proved frivolous to my ultimate analysis. I decided to derive most of my insights from the text data alone absent other factors."),
                 p("Finally, a quick note on the visualization of data: the processes of text mining and machine learning are incredibly intensive. It took hours of configuring the GPU on my high-end laptop for processes to run, and even then it takes hours. As such, most of the plots and displays of data will be in the form of images rather than graphs generated from this application, as I want it to actually display for people without overloading Shiny or somebody else's computer."),
                 a(href="https://github.com/willschrepf/state_of_the_union_data", "You can check out all the code for this app in my github repo!"),
                 h3("About Me"),
                 p("My name is Will Schrepferman and I study Government with a focus in Data Science! I am fascinated by political rhetoric and the history of the American presidency. This project has been a deeply interesting combination of those interests with data science techniques! They might seem somewhat arbitrary and purposeless, but sentiment analysis, topic modeling, and NLG have powerful implications for the real world. If these processes are designed at scale, they are able to be used to identify the authors of documents in a field like forensics, and NLG is even used to generate machine-written news content with minimal input from humans at mainstream news sites like Bloomberg. I hope you enjoy my project. You can reach me at willschrepferman@college.harvard.edu.")),
        tabPanel("Sentiment Analysis",
                 titlePanel("Sentiment Analysis"),
                 p("A sentiment analysis quantifies the positive and negative tone inherent in a speech. Using a comprehensive lexicon of words, I calculated the average positive sentiment of every State of the Union was calculated. In order to maintain the lexicon's viability, I only used 'modern' speeches- those delivered in a spoken format in the last hundred years (expanded slightly to include all of Pres. Wilson's speeches)- so that the lexicon did not struggle to analyze the diction and speech patterns of 19th and 18th century English."),
                 p("Hover over indiviudal data points to see more specifics! There are several interesting historical trends that can be seen here- Dwight Eisenhower tended to be the most positive speaker, and FDR's addresses during the Depression and Second World War were the only speeches with a net negativity."),
                 plotlyOutput("sentiment_plot"),
                 p("Here is some more information on sentiment analysis if you want to explore the concept further: https://towardsdatascience.com/sentiment-analysis-concept-analysis-and-applications-6c94d6f58c17")),
        tabPanel("Topic Modeling",
                 titlePanel("Topic Modeling"),
                 p("Topic modeling is a method for unsupervised classification of documents used to find natural groups of items."),
                 p("The model I used specifically uses Latent Dirichlet allocation (LDA), LDA is a particularly popular method for fitting a topic model. It treats each document as a mixture of topics, and each topic as a mixture of words. This allows documents to “overlap” each other in terms of content, rather than being separated into discrete groups, in a way that mirrors typical use of natural language."),
                 p("Before we get to my topic model itself, here are a few insights I found along the way."),
                 h3("Distinctive Words"),
                 p("Here is an exhibition of the idea of a word's 'uniqueness.' Using a word's tf-idf (term frequency multiplied by inverse document frequency) as a measure of how distinctive a word is too a speech, here are graphs with it on the x-axis for four speeches from pivotal points in American history (The Great Recession, the Mexican-American War, The Great Depression, and the end of World War 2)."),
                 p("As you can see, Obama's 2008 address had high tf-idf values for words like 'lending', 'recession', and 'recovery'; for Polk, 'Mexico' and 'Texas'; for Roosevelt, 'recovery' and 'structure'; and for Truman, 'reconversion' and 'peacetime.' These are the words most distinct to these particular speeches, and they make sense in historical context."),
                 img(src = "fourspeeches_clean.png", align = "center"),
                 h3("Topic Models"),
                 p("Finally, here is a display of the topic models themselves. I built a model with 8 topics; a 'topic' can be thought of as a series of words that the algorithm detects to be frequently used together. A few things I did to clean this data- I filtered out common words like 'America' or 'Congress.' The x-axis of each graph is a measure of how often a particular word is used in a given topic."),
                 img(src = "topics.png", align = "center"),
                 p("Imperfectly, I would say that topic 1 includes words about military and foreign policy; 2 seems to concern general fiscal spending; 3 seems to contain domestic issues like taxes and jobs; 4 seems like references to structures like the Constitution or bureaucratic departments; 5 seems to concern war, particularly picking up on Polk's language about the Mexican War that we saw earlier; 6 seems to talk about labor and business; 7 appears to be economic foreign policy and other treaties; and 8 seems to concern public service. These are simply my interpretations, as topic modeling is quantitatively incredible in its analysis but imperfect in offering up qualitatively what its results mean.")),
        tabPanel("Natural Language Generation",
                 titlePanel("Natural Language Generation"),
                 h3("Introduction"),
                 p("For the last part of my project, I delved into machine learning and neural networks. I set out to use natural language generation (NLG) to 'write' a totally original, machine-created State of the Union."),
                 p("Due to the large amount of data involved, I kept the input data restricted to just speeches from President Obama. Thus, I call my creation RoBama."),
                 h3("Meet RoBama"),
                 img(src = "robama.png", align = "center"),
                 p("I will offer a more technical explanation of how exactly RoBama works later, but for now, RoBama can be thought of as a machine that read all of Obama's speeches and is learning to write an entirely new one. It does so in stages."),
                 h3("RoBama, Stage 1:"),
                 p("In its first stage, RoBama is spitting complete gibberish:"),
                 strong("ROBAMA, STAGE 1: essorns erendert theiath omoch and out that prosiene bactineve the stoathane and make wneot shate beame a contoseiy in and scing leat tax fiscelorger pays we keet our ooten gat canced the biling wath furldrist to sedjort contiseut to betaepy job lowt"),
                 p("Totally meaningless- RoBama almost sounds like a toddler, but it has learned a few words like 'out,' 'job,' and 'tax."),
                 h3("RoBama, Stage 3:"),
                 p("By stage 3, RoBama is a toddler. It has learned to put words together, but they still don't make much sense."),
                 strong("ROBAMA, STAGE 3: trength in in our our economy the servest with our families and companies and differens to rebusing and the reads and i will condere that we allead we prelent and we spending the trade to the past and some every a deficit that i ask the who has when "),
                 p("RoBama is starting to make a moderate amount of sense. It references 'families and companies' who are, as he describes 'rebusing.' Whatever that may mean."),
                 h3("RoBama, Stage 20:"),
                 p("Skipping to Stage 20, RoBama is sounding more like an edgy tween who just learned what politics is."),
                 strong("ROBAMA, STAGE 20: the stratem is the same spend in the world we're most because of americans the few expections that in the some of the world we're not just that is the world will and that's why i'm asks of americans that we must and that we must and that's why i'm a"),
                 p("There's some moderate coherence to what it's saying now. Americans in some of the world, RoBama roughly describes, we're not just that. That's why it's asking of 'Americans that we must and that's why I'm a...' presumably it meant to say 'awesome creation by Will Schrepferman.'"),
                 h3("RoBama, Stage 40"),
                 p("We now cut to Stage 40. RoBama is nearly all grown up."),
                 strong("ROBAMA, STAGE 40: the first time in the real of the mission leaders in the own than i'm all people to share in the future we should come a responsibility she was that the american people are show the banks that will require america as the last month i will the eager with"),
                 p("It starts to become apparent that I should have given RoBama some form of ability to punctuate. Alas. Regardless, it has still discovered that it is the mission of leaders to share in the future. This is actually a pretty presidential statement. Nice job, RoBama! It also said that the banks will require America, which is also very true."),
                 h3("RoBama, Stage 50:"),
                 p("By Stage 50 (the final stage I was able to run), RoBama was doing even greater things"),
                 strong("ROBAMA, FINAL STAGE (50): this congress to get the work to the worsumering the future we can support the parts of the rest of this constructions that the worston we will not and to the people to the work of americans we put to the people and that is when it we are americans"),
                 p("I agree, RoBama! This Congress needs to get to work on the future so that we can support construction. When we put to the people, that is when we are Americans!!!!"),
                 h3("Conclusion"),
                 p("Thus ends the saga of RoBama. I feel a bit like Victor Frankenstein- this creature I've built is not quite human, but as it learned more and more from the input data, it actually approached some semblance of humanity. RoBama even made more sense than certain presidents have at times."),
                 h3("More Technical Details"),
                 p("Here is a more technical description of natural language generation for those interested!"),
                 p("NLG trains a machine-learning algorithm over a wide swath of input text. The model I used is a character-level language model, which takes a single character and then predicts what character should come next, based on what it has 'learned' from the input data. It does this using a neural network, which can be thought of as an artificial set of 'neurons' (data points) with and the connections between them (in this case, which character is associated with what other character). Specifically, I made a deep-learning LTSM (long term short memory) neural network (which is special because it can handle sequences of data) and trained it on my State of the Union data."),
                 p("I ran into several obstacles: first of all, in order to use machine learning in R in any amount of reasonable time, I had to set reconfigure the program to run on my GPU (graphical processing unit). Furthermore, I did not have enough RAM to take all of my data as input; so, I had to limit my input data to only one president's worth of speech data. I chose Obama. Finally, I was limited in how much data I could output, so each individual output is only 250 characters. Despite the difficulties, this proved to be a fascinating exploration of cutting-edge data science techniques."))),
    server = function(input, output) {
        
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

