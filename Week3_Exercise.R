
pti <- c("shiny","tidyverse","ggplot2movies")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
    install.packages(pti)
}

##########
### Shiny starter code
##########
library(shiny)
library(tidyverse)
library(ggplot2movies)

# Set randomness seed
set.seed(61)
# Prepare data
shiny_movie_set <- 
    movies %>% 
    filter(year >= 2000) %>%
    select(title,year,length,rating,votes,Action:Short) %>% 
    gather(genre,value,Action:Short) %>% 
    filter(value == 1) %>% 
    select(-value)

# Get genre list
genres <- 
    shiny_movie_set %>% 
    distinct(genre) %>% 
    unlist(.)

names(genres) <- NULL
minvotes<-sum(shiny_movie_set$votes)

# Define UI for application that draws a scatter plot for ggplot2movies dataset
ui <- fluidPage(

    # Application title
    titlePanel("Movie Length and IMDB scores"),

    # Sidebar with a slider input for the years, genre and number of votes
    sidebarLayout(
        sidebarPanel(
            sliderInput("years",
                        "Years",
                        min = 2000,
                        max = 2005,
                        value = c(2001,2004)),
            #select genre
            selectInput(inputId = "genre",
                        label="Genre",
                        choices = c("All",genres),
                        selected = "All"),
            sliderInput("minvotes",
                        "at least x votes",
                        min = min(shiny_movie_set$votes),
                        max = max(shiny_movie_set$votes),
                        value =  median(shiny_movie_set$votes))
        ),

        # Show the plot
        mainPanel(
            
            plotOutput("moviePlot")
        )
    )
)

# Define server logic required to draw a scatter plot
server <- function(input, output) {
    output$moviePlot<-renderPlot({
        
        print(input$years)
        print(input$genre)
        print(input$minvotes)
        
        #filter according to the input values
        plot_df<-shiny_movie_set %>% 
                  filter(year >= input$years[1] & year <= input$years[2]) %>% 
                  filter (votes >= input$minvotes)
        if(input$genre!="All"){
            plot_df<-plot_df %>% filter(genre==input$genre)
        }
            
        ggplot(plot_df,aes(x=length,y=rating,color=genre))+geom_point()
 })
}


# Run the application 
shinyApp(ui = ui, server = server)
