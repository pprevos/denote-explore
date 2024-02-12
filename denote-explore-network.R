#!/usr/bin/r
args <- commandArgs(trailingOnly = TRUE)

## args <- c("~/Documents/projects/denote-explore/test/network-files/denote-network.json",
##           "~/Documents/projects/denote-explore/test/network-files/denote-network.html")

if (length(args) != 2)
    stop("Need three arguments.")

# File locations
json_file <- args[1]
output_file <- args[2]

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
edges <- fromJSON(json_file)$links

vertices <- fromJSON(json_file)$nodes %>%
                              mutate(id = unlist(id))

edgelist <- vertices %>%
    select(from = id) %>%
    left_join(edges) %>%
    left_join(vertices, by = c("from" = "id")) %>%
    left_join(vertices, by = c("to" = "id")) %>% 
    select(title_from = name.x, title_to = name.y) %>% 
    distinct() %>%
    filter(!is.na(title_from) & !is.na(title_to))

# Create a graph from the edgelist
g <- graph_from_data_frame(d = edgelist, directed = TRUE)

# Isolated vertices
all_vertices <- vertices$name
connected_vertices_from <- unique(edgelist$title_from[!is.na(edgelist$title_to)])
connected_vertices_to <- unique(edgelist$title_to[!is.na(edgelist$title_to)])
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
