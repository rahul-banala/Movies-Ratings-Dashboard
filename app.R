#import necessary libraries
library(dplyr)
library(tidyr)
library(readr)
library(shiny)
library(ggplot2)

#Importing raw file for the title basics IMDb data set
movies_raw <- read_tsv(
  "~/Documents/Code/Movie_Ratings_Dashboard/data/title.basics.tsv",
  col_types = cols(
    tconst = col_character(),
    titleType = col_character(),
    primaryTitle = col_character(),
    originalTitle = col_character(),
    isAdult = col_integer(),
    startYear = col_integer(),
    endYear = col_integer(),
    runtimeMinutes = col_integer(),
    genres = col_character()
  ),
  na = "\\N"
)

#Filter data to only keep movies, excluding all other content types
movies <- movies_raw %>%
  filter(titleType == "movie")

#Clean the filtered movies data
clean_Movies <- movies %>%
  filter(!is.na(startYear) & !is.na(primaryTitle))

#Pull in the ratings.tsv data set
ratings_raw <- read_tsv(
  "~/Documents/Code/Movie_Ratings_Dashboard/data/title.ratings.tsv",
  col_types = cols(
    tconst = col_character(),
    averageRating = col_double(),
    numVotes = col_integer()
  ),
  na = "\\N"
)

#Clean ratings data
ratings_clean <- ratings_raw %>%
  filter(!is.na(averageRating) & !is.na(numVotes) & 
           averageRating >= 0 & averageRating <= 10 & 
           numVotes >= 0)

#Merge the two datasets
movies_ratings <- clean_Movies %>%
  left_join(ratings_clean, by = 'tconst')

#Check the merged data set
print(paste("Number of rows in merged dataset:", nrow(movies_ratings)))
print(paste("Number of columns in merged dataset:", ncol(movies_ratings)))

#List the number of NA values in each column as percentage
na_summary <- movies_ratings %>%
  summarise(across(everything(), ~sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "column", values_to = "missing_amount")

print(na_summary)

#Create flags for NA values
flagged_movies <- movies_ratings %>%
  mutate(
    has_end_year = !is.na(endYear),
    has_runtime = !is.na(runtimeMinutes),
    has_genres = !is.na(genres),
    has_rating = !is.na(averageRating),
    has_votes = !is.na(numVotes)
  )

#Define UI, specifically the title, user inputs and main display
ui <- fluidPage(
  titlePanel("Movies & Genre Popularity by Year"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "year",
        label = "Select Year:",
        choices = sort(unique(movies_ratings$startYear))
      )
    ),
    mainPanel(
      plotOutput("genrePlot"), 
      tableOutput("topMovies")
    )
  )
)

#Define server function and logic with reactive filtering for user inputted year
server <- function(input, output) {
  filtered_data <- reactive({
    movies_ratings %>%
      filter(startYear == input$year)
  })
  
  #Creates bar chart of the number of movies in each genre
  output$genrePlot <- renderPlot({
    genre_counts <- filtered_data() %>%
      separate_rows(genres, sep = ",") %>%
      group_by(genres) %>%
      summarize(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    #creates the actual chart
    ggplot(genre_counts, aes(x = reorder(genres, count), y = count)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = paste("Genre Popularity in", input$year),
        x = "Genre",
        y = "Count of Movies"
      )
  })
  
#Creates the top 10 movies table
  output$topMovies <- renderTable({
    filtered_data() %>%
      filter(numVotes >= 1000) %>% #filters out movies with less than 1000 user reviews
      arrange(desc(averageRating)) %>%
      head(10) %>%
      #Defines the top 10 movies table columns
      select(
        Title = primaryTitle,
        Year = startYear,
        Rating = averageRating,
        Runtime = runtimeMinutes,
        Genres = genres
      )
  })
}

#Run the Shiny app
shinyApp(ui = ui, server = server)
