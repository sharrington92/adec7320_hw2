# source(file.path("Scripts/000 - Setup.R"))

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
    data.main.adj <- data.main %>% 
      filter(!(fips %in% counties.with.na$fips)) %>% 
      select(where(is.numeric))
    
    main.corr <- data.main.adj %>% 
      cor() 
    
    main.corr[which(abs(main.corr[,"death.rate"]) > .2),"death.rate"] %>% sort()
    
    # Most Correlated Variables
    {
      vars <- names(main.corr[which(abs(main.corr[,"death.rate"]) > .3),"death.rate"])
      
      pairs(data.main.adj[,vars])
       
    }
    
    # All Scatter plots
    {
      v1 <- seq(1, 70, 4)
      v2 <- v1 + 4
      v2[length(v2)] <- nrow(main.corr)
      for(i in seq_along(v1)){
        pairs(data.main.adj[,c(12, v1[i]:v2[i])])
      } 
    }
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
    
    age.corr[,"death.rate"] %>% sort()
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
    
    race.corr[,"death.rate"] %>% sort()
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
    
    hisp.corr[,"death.rate"] %>% sort()
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
    
    nonhisp.corr[,"death.rate"] %>% sort()
  }
  
}










