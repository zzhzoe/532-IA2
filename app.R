library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv")

genres <- c("pop","rap","rock","latin","r&b","edm")
features <- c("danceability","energy","mode","speechiness","acousticness","liveness","valence","loudness")

app$layout(
  dbcContainer(
    list(
      htmlH1('Dashr heroku deployment'),
      dccGraph(id='plot_3'),
      htmlBr(),
      htmlLabel('Popularity Slider'),
      dccRangeSlider(
        id = 'pop-slider',
        min = 0,
        max = 100,
        marks = list("0"="0", "25"="25","50"="50","75"="75","100"="100"),
        value = list(5,100)
      ),
      htmlBr(),
      htmlLabel('Music Genre Dropdown Menu'),
      dccDropdown(
        id='genre-select',
        options = genres %>% purrr::map(function(genre, pop,feature) list(label = genre, value = genre)),
        value=list("rock","pop"),
        multi=TRUE),
      htmlBr(),
      htmlLabel('Music Features'),
      htmlBr(),
      dccDropdown(
        id='features',
        options = features %>% purrr::map(function(feature, pop) list(label = feature, value = feature)),
        value="danceability",
        multi=FALSE)
      
    )
  )
)


app$callback(
  output('plot_3', 'figure'),
  list(input('genre-select', 'value'),
       input('pop-slider','value'),
       input("features", "value")
  ),
  function(genre, pop, feature){
    pop_min <- pop[1]
    pop_max <- pop[2]
    filtered_df <- df %>%
      filter(playlist_genre %in% genre) %>%
      filter(track_popularity >= pop_min & track_popularity <= pop_max)
    
    if (is.null(feature)|length(feature) == 0){
      feature<-"danceability"}
    #else{feature<-features[1]}
    
    filtered_df$`Music Genres` = filtered_df$playlist_genre
    
    
    p <- filtered_df %>% ggplot(aes(y = track_popularity,
                 x = !!sym(feature),
                 color = `Music Genres`)) +
      geom_point(alpha=0.5) +
      ylab("popularity") +
      ggthemes::scale_fill_tableau() +
      theme_bw() 
    p <- p + theme(legend.title=element_blank())
    ggplotly(p)
  }
)

app$run_server(host = '0.0.0.0')
