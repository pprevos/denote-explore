#!/usr/bin/r
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2)
    stop("Need arguments for Denote directory and output filename.")

# Locate files (Modify to suit your needs)
denote_directory <- args[1]

# Output file
output_file <- args[2]

# Install required packages
req_packages <- c("stringr","dplyr", "igraph", "networkD3", "htmlwidgets")

for (i in req_packages) { #Installs packages if not yet installed
    if (! i %in% row.names(installed.packages()))
        install.packages(i, repos = "http://cran.us.r-project.org")
}

## Initialise libraries.
library(stringr)     # String manipulation
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)  # Data manipulation
library(igraph, quietly = TRUE, warn.conflicts = FALSE) # Network analysis
library(networkD3)   # Visualise networks with D3.js
library(htmlwidgets, quietly = TRUE, warn.conflicts = FALSE) # Save webpage to disk

## List denote files
denote_files <- list.files(denote_directory,
                           recursive = TRUE,
                           full.names = TRUE,
                           pattern = "[0-9]{8}T[0-9]{6}--.*__.*")

if (length(denote_files) == 0)
    stop("No Denote files found.")

## Create table of files from filenames
denotes <- tibble(filename   = denote_files,
                  identifier = str_extract(filename, "[0-9]{8}T[0-9]{6}"),
                  title      = str_extract(filename, "(?<=\\-\\-).*(?=__)"),
                  tags       = str_extract(filename, "(?<=\\_\\_).*(?=\\.)"),
                  filetype   = str_extract(filename, "\\..*$")) %>%
  mutate(title = str_replace_all(title, "-", " "),
         title = str_to_title(title))

# Read all text files to harvest links
text_denotes <- filter(denotes, filetype %in% c(".org", ".md", ".txt"))

links <- list()

for (f in text_denotes$filename) {
  l <- str_extract(readLines(f), "(?<=denote:)[0-9]{8}T[0-9]{6}")
  if (any(!is.na(l)))
    links[[f]] <- data.frame(
      from = str_extract(f, "[0-9]{8}T[0-9]{6}"),
      to = l[!is.na(l)])
}

# Collapse and clean list
network <- bind_rows(links)
row.names(network) <- NULL

# Remove empty duplicates and self-referencing links
network <- network[!duplicated(network) | network$from == network$to, ]

# Replace ids with names (remove links to attachments)
network_names <- network %>%
  left_join(text_denotes, by = c("from" = "identifier")) %>%
  left_join(text_denotes, by = c("to" = "identifier")) %>%
  select(from = title.x, to = title.y) %>%
  filter(!is.na(to))

# Create network and visualise

p <- simpleNetwork(network_names,
                   height="100px", width="100px",        
                   Source = "from", Target = "to",
                   linkDistance = 10,
                   charge = -900,
                   fontSize = 20,
                   fontFamily = "sans",
                   linkColour = "#000000",
                   nodeColour = "dodgerblue",
                   opacity = 1,
                   zoom = TRUE)

# Save HTML and JS to disk
saveWidget(p, file = output_file)









