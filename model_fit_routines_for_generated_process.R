library("MSGARCH")

set.seed(22)

ms2.garch.n <- CreateSpec(variance.spec = list(model = "sGARCH"),distribution.spec = list(distribution = "norm"),switch.spec = list(K = 2))

periods = c(500, 750, 1000, 1250, 2500, 5000, 10000, 50000)

periods = c(50000)
params = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35)
params = c(0.2, 0.25)
generate_s <- function(length, p_00, p_11) { 
  s = numeric(length) 
  s[1] = 0
  
  for (t in 2:length) { 
    random_0_1 = runif(1) 
    if (s[t-1] == 0) { 
      if (random_0_1 < p_00) { 
        s[t] = 0 
      } 
      else { 
        s[t] = 1 
      } 
    } 
    else { 
      if (random_0_1 < p_11) { 
        s[t] = 1 
      } 
      else { 
        s[t] = 0 
      } 
    } 
  }
  return(s) 
}

generate_process <- function(length, alpha_0_1, alpha_0_2, alpha_1_1, alpha_1_2, beta_1_1, beta_1_2, p_00, p_11){
  innovations = rnorm(length, mean = 0, sd = 1)
  s = generate_s(length, p_00, p_11)
  
  process = numeric(length) 
  process[1] = 0.01
  
  sigma_squared = numeric(length) 
  sigma_squared[1] = 0.01
  
  for (t in 2:length){
    if (s[t] == 0) {
      sigma_squared[t] = alpha_0_1 + alpha_1_1 * (process[t - 1] ** 2) + beta_1_1 * sigma_squared[t - 1]
      process[t] = innovations[t] * sqrt(sigma_squared[t])
    }
    else {
      sigma_squared[t] = alpha_0_2 + alpha_1_2 * (process[t - 1] ** 2) + beta_1_2 * sigma_squared[t - 1]
      process[t] = innovations[t] * sqrt(sigma_squared[t])
    }
  }
  return(process)
}

#For the MCMC fit procedure

for (param in params) {
  
  p_11 = 0.99
  p_22 = 0.99
  
  alpha_0_1 = 0.008
  alpha_1_1 = param
  beta_1_1 = 0.995 - param
  
  alpha_0_2 = 0.02
  alpha_1_2 = alpha_1_1
  beta_1_2 = beta_1_1
  
  for (t in periods) {
    alpha_0_1_bootstrap = c()
    alpha_1_1_bootstrap = c()
    beta_1_1_bootstrap = c()
    
    alpha_0_2_bootstrap = c()
    alpha_1_2_bootstrap = c()
    beta_1_2_bootstrap = c()
    
    p_1_1_bootstrap = c()
    p_2_2_bootstrap = c()
    
    for (n in 1:100) {
      process = generate_process(length = t, alpha_0_1, alpha_0_2, alpha_1_1, alpha_1_2, beta_1_1, beta_1_2, p_11, p_22)
      fit.mcmc <- FitMCMC(spec = ms2.garch.n, data = process)
      
      alpha_0_1_bootstrap[n] = mean(fit.mcmc$par[1:1000,1])
      alpha_1_1_bootstrap[n] = mean(fit.mcmc$par[1:1000,2])
      beta_1_1_bootstrap[n] = mean(fit.mcmc$par[1:1000,3])
      
      alpha_0_2_bootstrap[n] = mean(fit.mcmc$par[1:1000,4])
      alpha_1_2_bootstrap[n] = mean(fit.mcmc$par[1:1000,5])
      beta_1_2_bootstrap[n] = mean(fit.mcmc$par[1:1000,6])
      
      p_1_1_bootstrap[n] = mean(fit.mcmc$par[1:1000,7])
      p_2_2_bootstrap[n] = mean(1 - fit.mcmc$par[1:1000,8])
    }
    cat("T = ", t, "\n")
    cat("alpha_0_1 : ", alpha_0_1, " ", mean(alpha_0_1_bootstrap), " ", sd(alpha_0_1_bootstrap), "\n")
    cat("alpha_1_1 : ", alpha_1_1, " ", mean(alpha_1_1_bootstrap), " ", sd(alpha_1_1_bootstrap), "\n")
    cat("beta_1_1 : ", beta_1_1, " ", mean(beta_1_1_bootstrap), " ", sd(beta_1_1_bootstrap), "\n")
    
    cat("alpha_0_2 : ", alpha_0_2, " ", mean(alpha_0_2_bootstrap), " ", sd(alpha_0_2_bootstrap), "\n")
    cat("alpha_1_2 : ", alpha_1_2, " ", mean(alpha_1_2_bootstrap), " ", sd(alpha_1_2_bootstrap), "\n")
    cat("beta_1_2 : ", beta_1_2, " ", mean(beta_1_2_bootstrap), " ", sd(beta_1_2_bootstrap), "\n")
    
    cat("p_1_1 : ", p_11, " ", mean(p_1_1_bootstrap), " ", sd(p_1_1_bootstrap), "\n")
    cat("p_2_2 : ", p_22, " ", mean(p_2_2_bootstrap), " ", sd(p_2_2_bootstrap), "\n")
  }
  
}


#For the ML fit procedure

for (param in params) {
  
  p_11 = 0.99
  p_22 = 0.99
  
  alpha_0_1 = 0.008
  alpha_1_1 = param
  beta_1_1 = 0.995 - param
  
  alpha_0_2 = 0.02
  alpha_1_2 = alpha_1_1
  beta_1_2 = beta_1_1
  
  for (t in periods) {
    alpha_0_1_bootstrap = c()
    alpha_1_1_bootstrap = c()
    beta_1_1_bootstrap = c()
    
    alpha_0_2_bootstrap = c()
    alpha_1_2_bootstrap = c()
    beta_1_2_bootstrap = c()
    
    p_1_1_bootstrap = c()
    p_2_2_bootstrap = c()
    
    for (n in 1:100) {
      process = generate_process(length = t, alpha_0_1, alpha_0_2, alpha_1_1, alpha_1_2, beta_1_1, beta_1_2, p_11, p_22)
      fit.ml <- FitML(spec = ms2.garch.n, data = process)
      
      alpha_0_1_bootstrap[n] = fit.ml$par[1]
      alpha_1_1_bootstrap[n] = fit.ml$par[2]
      beta_1_1_bootstrap[n] = fit.ml$par[3]
      
      alpha_0_2_bootstrap[n] = fit.ml$par[4]
      alpha_1_2_bootstrap[n] = fit.ml$par[5]
      beta_1_2_bootstrap[n] = fit.ml$par[6]
      
      p_1_1_bootstrap[n] = fit.ml$par[7]
      p_2_2_bootstrap[n] = 1 - fit.ml$par[8]
    }
    cat("T = ", t, "\n")
    cat("alpha_0_1 : ", alpha_0_1, " ", mean(alpha_0_1_bootstrap), " ", sd(alpha_0_1_bootstrap), "\n")
    cat("alpha_1_1 : ", alpha_1_1, " ", mean(alpha_1_1_bootstrap), " ", sd(alpha_1_1_bootstrap), "\n")
    cat("beta_1_1 : ", beta_1_1, " ", mean(beta_1_1_bootstrap), " ", sd(beta_1_1_bootstrap), "\n")
    
    cat("alpha_0_2 : ", alpha_0_2, " ", mean(alpha_0_2_bootstrap), " ", sd(alpha_0_2_bootstrap), "\n")
    cat("alpha_1_2 : ", alpha_1_2, " ", mean(alpha_1_2_bootstrap), " ", sd(alpha_1_2_bootstrap), "\n")
    cat("beta_1_2 : ", beta_1_2, " ", mean(beta_1_2_bootstrap), " ", sd(beta_1_2_bootstrap), "\n")
    
    cat("p_1_1 : ", p_11, " ", mean(p_1_1_bootstrap), " ", sd(p_1_1_bootstrap), "\n")
    cat("p_2_2 : ", p_22, " ", mean(p_2_2_bootstrap), " ", sd(p_2_2_bootstrap), "\n")
  }
  
}
