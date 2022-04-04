# Load Cleaned Data ----
{
  my.data.list <- readRDS(file.path(folder.data.saved, "Cleaned Data.RDS"))
  
  names(my.data.list)
  
  # Turn list of datasets into individual datasets by my.data.list names
  for(i in seq_along(my.data.list)) {
    assign(names(my.data.list)[i], my.data.list[[i]])
  }
}


# LM #1 ----
{
  ## Model Estimation ----
  {
    counties.with.na <- data.main[rowSums(is.na(data.main)) > 0, ] %>% 
      select(fips, tot_pop)
    
    data.all <- inner_join(
      x = data.main,
      y = demographics.races.prop,
      by = "fips"
    ) %>% 
      filter(!(fips %in% counties.with.na$fips))
    
    View(colnames(data.all) %>% as.data.frame())
    
    model <- data.all %>% 
      lm(
        data = .,
        death.rate ~ case.rate  
        + age.avg_pop
        + tot_male_prop
        + administered_dose1_pop_pct
        + series_complete_pop_pct
        + booster_doses_vax_pct
        + as.factor(metro_status)
        + wa_male
        + ba_male
        + ia_male 
        + aa_male
        + na_male
      ) 
    
    model %>% 
      summary()
    
    plot(model)
  }
  
  # Diagnostics ----
  {
    ## Added Variable Plots ----
    {
      
      partial_reg_plot <- function(model, x){
        
        mm <- model.matrix(model)
        
        y <- model$residuals + model$fitted.values
        
        mm.names <- setdiff(colnames(mm), c(x, "(Intercept)"))
        
        d <- lm(data = as.data.frame(mm[,mm.names]), y ~ .)
        m <- lm(data = as.data.frame(mm[,mm.names]), mm[,x] ~ .)
        
        # plot(residuals(m), residuals(d), xlab = paste0(x, " residuals"), ylab = paste0("Vibration residuals"))
        # abline(0, coef(model)[x])
        
        lm.temp <- lm(d$residuals ~ m$residuals)
        
        gg <- data.frame(
          h = hatvalues(lm.temp),
          residuals.standard = rstandard(lm.temp),
          residuals = residuals(lm.temp)
        ) %>% 
          rownames_to_column(var = "observation") %>% 
          mutate(
            leverage.point = ifelse(h > (4 / length(lm.temp$fitted.values)), 1, 0),
            outlier = ifelse(abs(residuals.standard) >= 2, 1, 0),
            bad.leverage = ifelse(outlier == 1 & leverage.point == 1, 1, 0),
            good.leverage = ifelse(outlier == 0 & leverage.point == 1, 1, 0),
            d.resid = residuals(d),
            m.resid = residuals(m)
          ) %>% 
          ggplot(aes(x = m.resid, y = d.resid)) +
          geom_abline(intercept = 0, slope = coef(model)[x]) +
          # geom_smooth(method = "lm") +
          geom_point(aes(shape = as.factor(bad.leverage), color = abs(residuals.standard), size = h)) +
          ggrepel::geom_text_repel(aes(label = ifelse(bad.leverage == 1, observation, "")), size = 3) +
          scale_color_gradient(low = "blue4", high = "red2") +
          labs(color = "Standardized Residuals:", shape = "Bad Leverage:", size = "Leverage:") +
          xlab(paste0(x, " residuals")) +
          ylab(paste0("Vibration residuals")) +
          ggtitle("Partial Regression Plot") +
          theme_bw() + 
          theme(
            legend.position = "bottom",
            plot.title = element_text(hjust = .5),
            panel.grid.major = element_line(color = "gray70")
          )
        
        ggExtra::ggMarginal(
          gg, type = "histogram", margins = "both", size = 10, 
          xparams = list(bins = nrow(gg$data)/10), 
          yparams = list(bins = nrow(gg$data)/10)
        ) %>% 
          return()
      }
      
      
      names(model$coefficients)
      
      partial_reg_plot(model, names(model$coefficients)[12])
      
      # for(i in 2:(model$rank)){
      #   partial_reg_plot(model, names(model$coefficients)[i]) %>% 
      #     print()
      #   # ggsave(filename = paste0("Visualizations/Partial Regression/", names(model$coefficients)[i], ".png"))
      # }
    }
    
    
    # Partial Residual
    {
      car::crp(model)
    }
    
    
    # Heteroskedasticity ----
    {
      plot(model, which = 1)
      
      plot(model, which = 3)
      
      lm(residuals(model) ~ fitted(model)) %>% 
        summary()
    }
    
    
    # Normality ----
    {
      plot(model, which = 2)
      
      shapiro.test(residuals(model))
    }
    
    
    # Correlated Errors ----
    {
      forecast::checkresiduals(model)
      
      n <- length(residuals(model))
      
      plot(
        residuals(model)[-n], residuals(model)[-1], 
        xlab = expression(hat(epsilon)[i]), 
        ylab = expression(hat(epsilon)[i+1])
      )
      
      
      data.training %>% 
        lmtest::dwtest(formula(model), data = .)
    }
    
    
    # Collinearity ----
    {
      round(cor(model.matrix(model)[,-1]), 4)
      
      {
        
        x <- model.matrix(model)[,-1]
        e <- eigen(t(x) %*% x)
        
        e$values
        
        sqrt(e$values[1] / e$values)
      }
      
      vif(model.matrix(model)[,-1])
    }
  }
}