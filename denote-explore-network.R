#!/usr/bin/r

# Install required packages
req_packages <- c("stringr","dplyr", "igraph", "networkD3", "htmlwidgets")
for (i in req_packages) { #Installs packages if not yet installed
    if(! i%in% row.names(installed.packages())) install.packages(i)
}

## Initialise libraries
library(stringr)     # String manipulation
library(dplyr)       # Data manipulation
library(igraph)      # Network analysis
library(networkD3)   # Visualise networks with D3.js
library(htmlwidgets) # Save webpage to disk

# Locate files (Modify to suit your needs)
denote_directory <- "~/Documents/notes"

## List denote files
## Only Org files for now
files <- list.files(denote_directory,
                    recursive = TRUE,
                    full.names = TRUE,
                    pattern = ".org")
files <- files[!dir.exists(files)]

## Create table of files from filenames
denotes <- tibble(filename   = files,
                  identifier = str_extract(filename, "[0-9]{8}T[0-9]{6}"),
                  title      = str_extract(filename, "(?<=\\-\\-).*(?=__)"),
                  tags       = str_extract(filename, "(?<=\\_\\_).*(?=\\.)")) %>%
    mutate(title = str_replace_all(title, "-", " "),
           title = str_to_title(title)) %>%
    filter(!is.na(identifier))

# Read all files to harvest connections
links <- list()

for (f in denotes$filename) {
    l <- str_extract(readLines(f), "(?<=denote:)[0-9]{8}T[0-9]{6}")
    if (any(!is.na(l)))
        links[[f]] <- data.frame(
            from = str_extract(f, "[0-9]{8}T[0-9]{6}"),
            to = l[!is.na(l)])
}

# Collapse and clean list
network <- bind_rows(links)
row.names(network) <- NULL

# Remove empty links (non-org files)
network <- network[!duplicated(network), ]

# Remove any self-referencing links
network <- network[network$from != network$to, ]

# Replace ids with names (remove links to attachments)
network_names <- network %>%
    left_join(denotes, by = c("from" = "identifier")) %>%
    left_join(denotes, by = c("to" = "identifier")) %>%
    select(from = title.x, to = title.y) %>%
    filter(!is.na(to))

# Create network and visualise

p <- simpleNetwork(network_names,
                   height="100px", width="100px",        
                   Source = "from", Target = "to",
                   linkDistance = 10,
                   charge = -900,
                   fontSize = 40,
                   fontFamily = "sans",
                   linkColour = "#000000",
                   nodeColour = "dodgerblue",
                   opacity = 1, 
                   zoom = TRUE)

# Save HTML and JS to disk
saveWidget(p, file = paste0("~/denote-network.html"))
