# Load Cleaned Data
{
  my.data.list <- readRDS(file.path(folder.data.saved, "Cleaned Data.RDS"))
  
  names(my.data.list)
  
  # Turn list of datasets into individual datasets by my.data.list names
  for(i in seq_along(my.data.list)) {
    assign(names(my.data.list)[i], my.data.list[[i]])
  }
}

# Finding NA Values
{
  counties.with.na <- data.main[rowSums(is.na(data.main)) > 0, ] %>% 
    select(fips, tot_pop)
  
  inner_join(
    x = counties,
    y = counties.with.na,
    by = "fips"
  ) %>% 
    View()
}

# Correlations
{
  # Data.main
  {
    main.corr <- data.main %>% 
      filter(!(fips %in% counties.with.na$fips)) %>% 
      select(where(is.numeric)) %>% 
      cor() 
    
    main.corr[,"death.rate"]
  }
  
  # With age proportions
  {
    age.corr <- inner_join(
      x = data.main %>% select(fips, death.rate),
      y = demographics.ages.prop,
      by = "fips"
    ) %>% 
      select(where(is.numeric)) %>% 
      cor() 
    
    age.corr[,"death.rate"]
  }
  
  # With race proportions
  {
    race.corr <- inner_join(
      x = data.main %>% select(fips, death.rate),
      y = demographics.races.prop,
      by = "fips"
    ) %>% 
      select(where(is.numeric)) %>% 
      cor() 
    
    race.corr[,"death.rate"]
  }
  
  # With hispanic proportions
  {
    hisp.corr <- inner_join(
      x = data.main %>% select(fips, death.rate),
      y = demographics.hisp.prop,
      by = "fips"
    ) %>% 
      select(where(is.numeric)) %>% 
      cor() 
    
    hisp.corr[,"death.rate"]
  }
  
  # With non-hispanic proportions
  {
    nonhisp.corr <- inner_join(
      x = data.main %>% select(fips, death.rate),
      y = demographics.nonhisp.prop,
      by = "fips"
    ) %>% 
      select(where(is.numeric)) %>% 
      cor() 
    
    nonhisp.corr[,"death.rate"]
  }
  
}










