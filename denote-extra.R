## Initialise libraries
library(stringr) # String manipulation
library(dplyr)

## List denote files
denotes <- list.files(recursive = TRUE, full.names = TRUE)
denotes <- denotes[!dir.exists(denotes)]

## Create table of files
notes <- tibble(filename   = denotes,
                identifier = str_extract(filename, "[0-9]{8}T[0-9]{6}"),
                title      = str_extract(filename, "(?<=\\-\\-).*(?=__)"),
                tags       = str_extract(filename, "(?<=\\_\\_).*(?=\\.)")) %>%
  mutate(title = str_replace_all(title, "-", " "),
         title = str_to_title(title)) %>%
  filter(!is.na(identifier))

# Read all files to harvest connections
links <- list()

for (f in notes$filename) {
  l <- str_extract(readLines(f), "(?<=denote:)[0-9]{8}T[0-9]{6}")
  if (any(!is.na(l)))
    links[[f]] <- data.frame(from = str_extract(f, "[0-9]{8}T[0-9]{6}"),
                             to = l[!is.na(l)])
}

# Collapse and clean list
network <- do.call(rbind, links)
row.names(network) <- NULL

# Create network and visualise
library(igraph)

g <- graph_from_data_frame(network)

plot(g,
     label = V(g)$label,
     layout = layout.kamada.kawai,
     edge.arrow.size = .5,
     vertex.frame.color = NA,
     vertex.color = "lightgrey",
     vertex.labal.family = "sanserif",
     vertex.label.color = "black")
notes
f <- notes$filename[10]
