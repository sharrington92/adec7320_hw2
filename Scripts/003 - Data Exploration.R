# Load Cleaned Data
{
  my.data.list <- readRDS(file.path(folder.data.saved, "Cleaned Data.RDS"))
  
  names(my.data.list)
  
  # Turn list of datasets into individual datasets by my.data.list names
  for(i in seq_along(my.data.list)) {
    assign(names(my.data.list)[i], my.data.list[[i]])
  }
}