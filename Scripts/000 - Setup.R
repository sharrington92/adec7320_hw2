# Packages
{
  library(tidyverse)
  library(scales)
  library(lubridate)
}

# Folder Structure
{
  my.path <- file.path(getwd(), "HW/HW2/")
  
  if(!dir.exists(file.path(my.path, "Raw Data"))){
    dir.create(file.path(my.path, "Raw Data"))
  }
  folder.data.raw <- file.path(my.path, "Raw Data")
  
  if(!dir.exists(file.path(my.path, "Saved Data"))){
    dir.create(file.path(my.path, "Saved Data"))
  }
  folder.data.saved <- file.path(my.path, "Saved Data")
  
  if(!dir.exists(file.path(my.path, "Scripts"))){
    dir.create(file.path(my.path, "Scripts"))
  }
  folder.scripts <- file.path(my.path, "Scripts")
}
