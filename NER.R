#named entrity recognition and geocoding tutorial
#https://towardsdatascience.com/quick-guide-to-entity-recognition-and-geocoding-with-r-c0a915932895
library(gutenbergr)
library(tidyverse)
library(entity)
library(tidygeocoder)
library(sf)
library(osmdata)
library(magrittr)
library(readtext)
library(tidytext)
library(spacyr)
library(stringr)
library(raster)
#download the book by using the gutenberg id
thru_casentino <- gutenberg_download(57404, 
                                     mirror = "ftp://eremita.di.uminho.pt/pub/gutenberg/") 
#remove the id column
thru_casentino %<>% select(-gutenberg_id) 
