# REGRESSIONS

lm_simulator <- function(data = NULL, dep_var = NULL, indep_var = NULL, key = NULL){
 
 models <- lapply( unlist(unique(data[,key])) , function(z){
  data_sp <- data %>%
   filter(data[,as.character(colnames(data)[key])] == z)
  
  m <- lm(as.formula(paste0(colnames(data_sp)[dep_var],"~", paste0(colnames(data_sp)[indep_var], collapse = "+"))), 
          data = data_sp)
  
  b0 <- cbind("Intercept" = m$coefficients[1], "p.value" = summary(m)$coefficients[1,4])
  b1 <- cbind("Intercept" = m$coefficients[2], "p.value" = summary(m)$coefficients[2,4])
  residuals <- m$residuals
  fitted_val <- m$fitted.values
  test = dwtest(m)
  adj_r2 = summary(m)$adj.r.squared
  r2 = summary(m)$r.squared
  tstat = t(summary(m)$coefficients[,3])
  
  return(list("intercept" = b0, "beta" = b1, "residuals" = residuals, "fitted" = fitted_val, 
              "model" = m, "dwtest" = test, "adj_r2" = adj_r2, "r2" = r2, "tstat" = tstat))  }) #end of lapply
 names(models) <- unlist(unique(data[,key]))
 
 models_table <- lapply(1:length(models), function(z){
  r_f <- cbind("countries" = rep(unlist(unique(data[,key]))[z]), 
               "time" = c("2018-07-01", "2018-10-01", "2019-01-01", "2019-04-01", "2019-07-01", "2019-10-01"),
               "residuals" = models[[z]]$residuals, 
               "fitted" = models[[z]]$fitted)
  return(r_f)
 }); names(models_table) <- unlist(unique(data[,key]))
 return(list("tables" = models_table, "model_output" = models))
}
