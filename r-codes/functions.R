get_data <- function(query){
  res<- dbExecute(con, query)
  return(dbFetch(res))
}


source("r-codes/lm_simulator.R")

estimate_population_size <- function(data, country, quarter, quarter_days, trunc, bootstrap = TRUE) {
  res <- data[idcountry == country & eval(as.name(quarter)) & eval(as.name(quarter_days)) <= trunc, .N, general_id][, .(times = .N), keyby=N] 
  
  ll_trun_pois <- function(x, par) {
    vec <- as.vector(table(x))
    lev <- as.numeric(names(table(x)))
    
    den <- 1 - dpois(x = 0, lambda = par) - dpois(x = 1, lambda = par)
    probs <- dpois(x = lev, lambda = par)
    ll <- vec * log(probs / den)
    sum(ll)
  }
  
  ll_trun_geom <- function(x, par) {
    vec <- as.vector(table(x))
    lev <- as.numeric(names(table(x)))
    den <- 1 - dgeom(x = 0, prob = par) - dgeom(x = 1, prob = par)
    probs <- dgeom(x = lev, prob = par)
    ll <- vec * log(probs / den)
    sum(ll)
  }
  
  ll_trun_negbin <- function(x, par) {
    vec <- as.vector(table(x))
    lev <- as.numeric(names(table(x)))
    den <- 1 - dnbinom(x = 0, mu = par[1], size = par[2]) - dnbinom(x = 1, mu = par[1], size = par[2])
    probs <- dnbinom(x = lev,
                     mu = par[1],
                     size = par[2])
    ll <- vec * log(probs / den)
    sum(ll)
  }
  
  yyy <- rep(res$N[-1], res$times[-1])
  
  est_pois <- maxLik(logLik = ll_trun_pois, start = 0.1, method = "NR", x = yyy)
  est_geom <- maxLik(logLik = ll_trun_geom, start = 0.1, method = "NR", x = yyy)
  #est_negb <- maxLik(logLik = ll_trun_negbin, start = c(coef(est_pois), 1), method = "NR", x = yyy)
  
  AICs <- sapply(list(est_pois,est_geom), AIC) # ,est_negb
  
  lev <- as.numeric(names(table(yyy)))
  
  fitted <- data.frame(vals = res$N[-1],
                       true = res$times[-1], 
                       poisson = round(dpois(x = lev, coef(est_pois))/ (1 - dpois(0, coef(est_pois)) - dpois(1, coef(est_pois)))*sum(res$times[-1])),
                       geom = round(dgeom(x = lev, coef(est_geom))/ (1 - dgeom(0, coef(est_geom)) - dgeom(1, coef(est_geom)))*sum(res$times[-1])))
  
  #negbin =  round(dnbinom(x = lev, mu=coef(est_negb)[1], size=coef(est_negb)[2])/
  #                (1-dnbinom(x = 0, mu=coef(est_negb)[1],size=coef(est_negb)[2])-
  #                dnbinom(x = 1, mu=coef(est_negb)[1],size=coef(est_negb)[2])))*sum(res$times[-1]))
  
  
  popsizes <- 
    c(poisson = dpois(0, coef(est_pois))*sum(res$times[-1])/(1 - dpois(0, coef(est_pois)) - dpois(1, coef(est_pois))) + sum(res$times),
      geom = dgeom(0, coef(est_geom))*sum(res$times[-1])/(1 - dgeom(0, coef(est_geom)) - dgeom(1, coef(est_geom))) + sum(res$times))
  #negbin = dnbinom(0, mu=coef(est_negb)[1], size=coef(est_negb)[2])*sum(res$times[-1])/(1 - dnbinom(0, mu=coef(est_negb)[1], size=coef(est_negb)[2]) - dnbinom(1, mu=coef(est_negb)[1], size=coef(est_negb)[2])) + sum(res$times))
  
  ## bootstrap only for poisson 
  pop_boot_est <- NULL
  if (bootstrap) {
    pop_size <- popsizes[1]
    probs <- c(pop_size- sum(res$times), res$times)/pop_size
    boots <- 200
    sampled_freqs <- rmultinom(n = boots, size = pop_size, prob = probs)
    sampled_freqs <- t(sampled_freqs)
    pop_boot_est <- numeric(boots)
    for (b in 1:boots) {
      yyy_boot <- rep(res$N[-1], sampled_freqs[b,-c(1,2)])
      est_boot <- maxLik(logLik = ll_trun_pois, start = coef(est_pois), method = "NR", x = yyy_boot)
      pop_boot_est[b] <- dpois(0, coef(est_boot))*sum(res$times[-1])/(1 - dpois(0, coef(est_boot)) - dpois(1, coef(est_boot))) + sum(res$times)
    }
  }
  
  
  return(list(popsizes = popsizes,
              #popsizes_sd = sqrt(sum((pop_boot_est-mean(pop_boot_est))^2)/boots),
              boots = pop_boot_est, 
              AICs = AICs,
              fitted = fitted,
              models = list(est_pois = coef(summary(est_pois)),
                            est_geom = coef(summary(est_geom))), 
              #est_negb = coef(summary(est_negb))),
              data = res))
}
