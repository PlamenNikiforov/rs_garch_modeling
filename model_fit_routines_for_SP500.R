library("MSGARCH")
library("writexl")
library("readxl")

set.seed(22)

library(readxl)
# To run the script change the read_excel path to the S&P 500.xlsx path on your local machine
S_P_500 <- read_excel("CHANGE THIS PATH", 
                      sheet = "Returns", col_types = c("date", 
                                                       "numeric"))
ms2.garch.n <- CreateSpec(variance.spec = list(model = "sGARCH"),distribution.spec = list(distribution = "norm"),switch.spec = list(K = 2))

res = c()
res_mcmc=c()
tw = 2500
for (t in 2:(length(S_P_500$`S&P 500`) - tw -1)) {
  current_data = S_P_500[t:(t + tw), 1:2]
  fit.ml <- FitML(spec = ms2.garch.n, data = rev(current_data$`S&P 500`))
  fit.mcmc <- FitMCMC(spec = ms2.garch.n, data = rev(current_data$`S&P 500`))
  vol <- Volatility(fit.ml)[tw]
  vol_mcmc <- Volatility(fit.mcmc)[tw]
  res = append(res, vol)
  res_mcmc = append(res_mcmc, vol_mcmc)
}






