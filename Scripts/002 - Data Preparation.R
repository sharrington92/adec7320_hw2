# source(file.path(folder.scripts, "001 - Import Data.R"))

# Prep Vaccination Dataset ----
{
  vaccines <- vaccinations.raw %>% 
    filter(mdy(Date) == ymd("2022-03-31")) %>% 
    rename_with(.fn = str_to_lower) %>% 
    select(-c(date, mmwr_week, recip_county, recip_state)) %>% 
    select(!contains("svi") & !contains("equity"))
  
  colnames(vaccines)
}

# Prep COVID Data ----
{
  covid <- covid.raw %>% 
    filter(ymd(date) < ymd("2022-04-01")) %>% 
    group_by(fips) %>% 
    summarize(across(c(cases, deaths), sum))
}

# Prep County Demographics ----
{
  ## Get lookup table for age groupings to actual ages ----
  demo.age.lookup <- data.frame(
    agegrp = c(0:18),
    ages = c("All", paste0(seq(0, 80, 5), "-", seq(4, 84, 5)), "85+")
  )
  
  
  demographics <- demographics.raw %>% 
    as_tibble() %>% 
    rename_with(.fn = str_to_lower) %>% 
    filter(year == 12) %>% 
    mutate(
      fips = str_c(state, county),
      year = 2019
    ) %>% 
    left_join(
      y = demo.age.lookup, by = "agegrp"
    ) %>% 
    mutate(
      ages = factor(ages, levels = demo.age.lookup$ages)
    ) %>% 
    select(-c(sumlev, agegrp, state, county, stname, ctyname, year)) %>% 
    pivot_longer(-c(fips, ages), names_to = "variable", values_to = "value") %>% 
    separate(variable, into = c("category", "sex"), sep = "\\_")
  
  # tot = sum(wa, ba, ia, aa, na, tom) = sum(nh, h)
  # nh = sum(nhwa, nhba, nhia, nhaa, nhna, nhtom)
  # h = sum(hwa, hba, hia, haa, hna, htom)
  
  # Vectors to be used to divide dataset into list of dataset
  # Categories ending with "c" are not additive and are being removed.
  races <- c("tot", "wa", "ba", "ia", "aa", "na", "tom")
  hispanic <- c("h", "hwa", "hba", "hia", "haa", "hna", "htom")
  not.hispanic <- c("nh", "nhwa", "nhba", "nhia", "nhaa", "nhna", "nhtom")
  
  # Separate data into 3 datasets based on above vectors ----
  demographics.list <- demographics %>% 
    filter(
      category %in% c(races, hispanic, not.hispanic)
    ) %>% 
    mutate(
      split = case_when(
        category %in% races ~ "race",
        category %in% hispanic ~ "hispanic",
        category %in% not.hispanic ~ "not.hispanic"
      )
    )  %>% 
    split(~ .$split + .$ages)
  
  
  demographics.list <- lapply(demographics.list, function(x){
    x %>% 
      select(-split) %>% 
      unite("category", category, sex) %>% 
      pivot_wider(names_from = category, values_from = value) 
  })
  
  
  # Add totals for hispanic/not hispanic datasets ----
  {
    # Find which datasets are hispanic/not
    nh.list <- names(demographics.list)[which(str_detect(names(demographics.list), "not.hispanic"))]
    h.list <- names(demographics.list)[which(!str_detect(names(demographics.list), "not.hispanic") & str_detect(names(demographics.list), "hispanic"))]
    r.list <- names(demographics.list)[which(str_detect(names(demographics.list), "race"))]
    
    # Loop through datasets adding total column
    for(i in seq_along(h.list)){
      demographics.list[[h.list[i]]] <- demographics.list[[h.list[i]]] %>% 
        mutate(h.total = h_male + h_female)
    }
    
    for(i in seq_along(nh.list)){
      demographics.list[[nh.list[i]]] <- demographics.list[[nh.list[i]]] %>% 
        mutate(nh.total = nh_male + nh_female)
    }
  }
  
  # Create dataset of age totals ----
  {
    demographics.ages <- demographics.list[r.list] %>% 
      do.call(rbind, .) %>% 
      select(fips, ages, tot_pop) %>% 
      pivot_wider(names_from = ages, values_from = tot_pop) %>% 
      rename(total = All)
    
    demographics.ages.prop <- demographics.ages %>% 
      mutate(across(
        -c(fips), function(x){x / total}
      ))
    
  }
  
  
  # Create dataset of race totals (sum ages) ----
  {
    demographics.races <- demographics.list[r.list] %>% 
      do.call(rbind, .) %>% 
      filter(ages == "All")
    
    demographics.races.prop <- demographics.races %>% 
      mutate(across(
        -c(fips, ages), function(x){x / tot_pop}
      ))
  }
  
  
  # Create dataset of hispanic/non totals (sum ages) ----
  {
    demographics.hisp <- demographics.list[h.list] %>% 
      do.call(rbind, .) %>% 
      filter(ages == "All")
    
    demographics.hisp.prop <- demographics.hisp %>% 
      left_join(
        y = demographics.ages %>% select(fips, total),
        by = "fips"
      ) %>% 
      mutate(across(
        -c(fips, ages), function(x){x / total}
      ))
  }
  
  
  # Create dataset of non-hispanic totals (sum ages) ----
  {
    demographics.nonhisp <- demographics.list[nh.list] %>% 
      do.call(rbind, .) %>% 
      filter(ages == "All")
    
    demographics.nonhisp.prop <- demographics.nonhisp %>% 
      left_join(
        y = demographics.ages %>% select(fips, total),
        by = "fips"
      ) %>% 
      mutate(across(
        -c(fips, ages), function(x){x / total}
      ))
  }
  
  # Create basic summary dataset ----
  {
    demographics.basic <- demographics.list[r.list] %>% 
      do.call(rbind, .) %>% 
      filter(ages == "All") %>% 
      group_by(fips) %>% 
      summarize(across(
        c(tot_pop, tot_male, tot_female), sum
      )) %>% 
      mutate(across(
        c(tot_male, tot_female), function(x){x / tot_pop}, .names = "{.col}_prop"
      )) %>% 
      left_join(
        # Average ages
        y = demographics.list[r.list] %>% 
          do.call(rbind, .) %>% 
          select(fips, ages, tot_pop, tot_male, tot_female) %>% 
          filter(ages != "All") %>% 
          pivot_longer(-c(fips, ages), values_to = "n") %>% 
          separate(ages, c("age.min", "age.max"), sep = "-|\\+", convert = TRUE) %>% 
          replace_na(list(age.max = 89)) %>% 
          group_by(fips, name) %>% 
          summarize( 
            age.avg = sum((age.min + age.max)/2 * n) / sum(n)
          ) %>% 
          pivot_wider(names_from = name, values_from = age.avg) %>% 
          rename(
            age.avg_female = tot_female,
            age.avg_male = tot_male,
            age.avg_pop = tot_pop
          ),
        by = "fips"
      )
  }
  
  
  # Create dataset of proportions by race and age ----
  {
    demographics.prop.list <- lapply(demographics.list[r.list], function(x){
      x %>% 
        left_join(
          y = demographics.basic %>%
            select(fips, tot_pop) %>% 
            rename(total.grand = tot_pop),
          by = "fips"
        ) %>% 
        mutate(across(
          -c(fips, ages), function(x){x / total.grand}
        ))
    })
    
  }
}

# County Dataset ----
{
  inc.counties <- base::intersect(
    covid %>% pull(fips),
    demographics.basic %>% pull(fips)
  ) %>% 
    base::intersect(
      vaccines %>% pull(fips)
    )
  
  # Counties excluded from covid
  exc.covid <- setdiff(covid$fips, inc.counties)
  
  # Counties excluded from demographics
  exc.demo <- setdiff(demographics$fips, inc.counties)
  
  # Counties excluded from vaccines
  exc.vacc <- setdiff(vaccines$fips, inc.counties)
  
  # All Excluded
  exc.all <- unique(c(exc.covid, exc.demo, exc.vacc)) %>% 
    sort()
  
  # Create counties dataset
  counties <- demographics.raw %>% 
    rename_with(.fn = str_to_lower) %>% 
    mutate(
      fips = str_c(state, county)
    ) %>% 
    select(fips, stname, ctyname) %>% 
    distinct() %>% 
    rename(state = stname, county = ctyname) %>% 
    mutate(
      included.in.all = ifelse(fips %in% inc.counties, 1,0),
      excluded.from.covid = ifelse(fips %in% exc.covid, 1,0),
      excluded.from.demo = ifelse(fips %in% exc.demo, 1,0),
      excluded.from.vacc = ifelse(fips %in% exc.vacc, 1,0)
    )
}

# Join COVID, vaccine, and basics demographics datasets ----
{
  data.main <- left_join(
      x = covid,
      y = demographics.basic,
      by = "fips"
    ) %>% 
    mutate(
      case.rate = cases / tot_pop,
      death.rate = deaths / tot_pop
    ) %>% 
    left_join(
      y = vaccines,
      by = "fips"
    ) %>% 
    filter(fips %in% inc.counties)
  
}


# Save cleaned data as a list of datasets ----
{
  my.data.list <- list(
    data.main, counties,
    demographics.ages, demographics.ages.prop, 
    demographics.races, demographics.races.prop,
    demographics.nonhisp, demographics.nonhisp.prop,
    demographics.hisp, demographics.hisp.prop,
    demographics.list, demographics.prop.list
  )
  
  names(my.data.list) <- c(
    "data.main", "counties",
    "demographics.ages", "demographics.ages.prop", 
    "demographics.races", "demographics.races.prop",
    "demographics.nonhisp", "demographics.nonhisp.prop",
    "demographics.hisp", "demographics.hisp.prop",
    "demographics.list", "demographics.prop.list"
  )
  
  saveRDS(my.data.list, file.path(folder.data.saved, "Cleaned Data.RDS"))
}
