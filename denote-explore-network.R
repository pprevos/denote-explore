#!/usr/bin/r
args <- commandArgs(trailingOnly = TRUE)

# args <- c("~/Documents/notes/graphs/denote-network.json")

if (length(args) != 1)
  stop("No load file found.")

# File locations
json_file <- args[1]
output_file <- paste0(tools::file_path_sans_ext(json_file), ".html")

# Install required packages
req_packages <- c("jsonlite", "tibble", "dplyr", "igraph",
                  "tidyr", "networkD3", "htmlwidgets")

for (package in req_packages) { #Installs packages if not yet installed
  if (! package %in% row.names(installed.packages()))
    install.packages(package, repos = "http://cran.us.r-project.org")
  library(package, character.only = TRUE,
          quietly = TRUE, warn.conflicts = FALSE)
}

# Read Data
json_data <- fromJSON(json_file)

# Create a graph
directed <- json_data$meta$directed
directed <- ifelse(is.null(directed), FALSE, directed)
edges <- tibble(json_data$edges)
vertices <- tibble(json_data$nodes)

edgelist <- vertices %>%
  select(source = id) %>%
  left_join(edges) %>%
  left_join(vertices, by = c("source" = "id")) %>%
  left_join(vertices, by = c("target" = "id")) %>%
  select(name_from = name.x, name_to = name.y) %>% 
  distinct()

# Create a graph from the edgelist
g <- graph_from_data_frame(d = filter(edgelist, !is.na(name_to)), directed = directed)

# Isolated vertices
all_vertices <- unique(edgelist$name_from)
connected_vertices_from <- unique(edgelist$name_from[!is.na(edgelist$name_to)])
connected_vertices_to <- unique(edgelist$name_to[!is.na(edgelist$name_to)])
isolated_vertices <- setdiff(all_vertices, c(connected_vertices_from, connected_vertices_to))
g <- add_vertices(g, length(isolated_vertices), name = isolated_vertices)

# plot(g, edge.arrow.size = 1)

wc <- cluster_walktrap(g)
members <- membership(wc)

h <- igraph_to_networkD3(g, group = members)

forceNetwork(Links = h$links, Nodes = h$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group',
             opacity = 1, fontSize = 20) %>% 
  saveNetwork(file = output_file, selfcontained = FALSE)
