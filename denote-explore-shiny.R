# Interactive Denote network

library(shiny)
library(stringr)     # String manipulation
library(dplyr)       # Data manipulation
library(igraph)      # Network analysis
library(networkD3)   # Visualise networks with D3.js
library(htmlwidgets) # Save webpage to disk
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Denote Statistics and Visualisation"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("denote_directory", label = "Denote directory", value = "~/Documents/notes/"),
      actionButton("read_files", "Read files"),
      textOutput("count_denotes"),
      textOutput("count_links"),
      selectInput("tags", label = h3("Keywords"), 
                         choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3))
    ),
    mainPanel(
      simpleNetworkOutput("network")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  denote_files <- eventReactive(input$read_files, {
    list.files(input$denote_directory,
               recursive = TRUE,
               full.names = TRUE,
               pattern = "[0-9]{8}T[0-9]{6}--.*__.*")
  })
  
  denotes <- reactive ({
    tibble(filename   = denote_files(),
           identifier = str_extract(filename, "[0-9]{8}T[0-9]{6}"),
           title      = str_extract(filename, "(?<=\\-\\-).*(?=__)"),
           tags       = str_extract(filename, "(?<=\\_\\_).*(?=\\.)"),
           filetype   = str_extract(filename, "\\..*$")) %>%
      mutate(title = str_replace_all(title, "-", " "),
             title = str_to_title(title))})
  
  denote_links <- reactive ({
    text_denotes <- filter(denotes(), filetype %in% c(".org", ".md", ".txt"))
    
    links <- list()
    for (f in text_denotes$filename) {
      l <- str_extract(readLines(f), "(?<=denote:)[0-9]{8}T[0-9]{6}")
      if (any(!is.na(l)))
        links[[f]] <- data.frame(
          from = str_extract(f, "[0-9]{8}T[0-9]{6}"),
          to = l[!is.na(l)])}
    
    network <- bind_rows(links)
    row.names(network) <- NULL
    network[network$from != network$to & !duplicated(network), ]})
  
  filetags <- reactive({
    denotes() %>% 
      mutate(tags = str_extract(tags, "(?=_).*")) %>% 
      select(identifier, tags) %>% 
      separate(tags, into = paste("tag", 1:9), sep = "_") %>% 
      pivot_longer(-identifier, values_to = "tag") %>% 
      select(-name) %>% 
      filter(!is.na(tag) & tag != "")})  
  
choices <- reactive({
  unique(filetags()$tag)})
  
output$count_denotes <- reactive({
    paste(length(denote_files()), "files")})
  
  output$count_links <- reactive({
    paste(nrow(denote_links()), "links")})
  
  output$network <- renderSimpleNetwork({
    simpleNetwork(denote_links(),
                  height="100px", width="100px",        
                  Source = "from", Target = "to",
                  linkDistance = 10,
                  charge = -900,
                  fontSize = 40,
                  fontFamily = "sans",
                  linkColour = "#000000",
                  nodeColour = "dodgerblue",
                  opacity = 1, 
                  zoom = TRUE)})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
