library(tidyverse)

# Loading in the master dataframe===

tarot_master <- read_csv("data/tarot_master.csv", col_types = "ccc")

# Setting reference vectors for cards====
tarotorder_ref <- tarot_master$Card

#Appending file extension ".png" onto names for sorting===
tarotorder_refpath <- sapply(tarotorder_ref, function(x) paste0(x,".png"))

# Loading in image files====
#Pulling image file names from folder===
imagepaths <- list.files("images")

#Reordering names in order of tarot===
imagepaths <- imagepaths[order(match(imagepaths,tarotorder_refpath))]

#Appending files names with directory===
imagepaths <- sapply(imagepaths, function(x) paste0("images/",x))

# Plugging the cards into the Tarot master Table====
tarotdeck <- tarot_master %>%
  mutate(Path = imagepaths)

# Pulling unique reference for card pulls====
mastercardset <- tarotorder_ref[1:(nrow(tarotdeck)/2)]

# Setting up a reference for the card positions====
reversalset <- c("Reversed","")
