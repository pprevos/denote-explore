#!/usr/bin/r
args <- commandArgs(trailingOnly = TRUE)

# args <- c("~/.config/emacs/denote-edges.json",
#           "~/.config/emacs/denote-vertices.json",
#           "~/denote-network.html")

if (length(args) != 3)
    stop("Need three arguments.")

# File locations
edges_file <- args[1]
vertices_file <- args[2]
output_file <- args[3]

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
edges <- fromJSON(edges_file) %>%
    enframe(name = "from", value = "to") %>%
    mutate(to = unlist(to))

vertices <- fromJSON(vertices_file) %>%
    enframe(name = "id", value = "title") %>%
    mutate(title = unlist(title))

edgelist <- vertices %>%
  select(from = id) %>%
  left_join(edges) %>%
  left_join(vertices, by = c("from" = "id")) %>%
  left_join(vertices, by = c("to" = "id")) %>%
  select(title_from = title.x, title_to = title.y) %>% 
  distinct()

# Create a graph from the edgelist
g <- graph_from_data_frame(d = filter(edgelist, !is.na(title_to)), directed = TRUE)

# Isolated vertices
all_vertices <- unique(edgelist$title_from)
connected_vertices_from <- unique(edgelist$title_from[!is.na(edgelist$title_to)])
connected_vertices_to <- unique(edgelist$title_to[!is.na(edgelist$title_to)])
isolated_vertices <- setdiff(all_vertices, c(connected_vertices_from, connected_vertices_to))
g <- add_vertices(g, length(isolated_vertices), name = isolated_vertices)

#plot(g, edge.arrow.size = 1)

wc <- cluster_walktrap(g)
members <- membership(wc)

h <- igraph_to_networkD3(g, group = members)

forceNetwork(Links = h$links, Nodes = h$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group',
             opacity = 1, fontSize = 20) %>% 
    saveNetwork(file = output_file)
