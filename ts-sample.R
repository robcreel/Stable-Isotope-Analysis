# https://smac-group.github.io/ts/introtimeseries.html
library(astsa)
library(mgcv)
library(simts)

# Load Global Temp Data, create grouped time series object (gts), and plot
data(globtemp, package = "astsa")
globtemp = gts(globtemp, start = 1880, freq = 1, unit_ts = "C", name_ts = "Global Temperature Deviations", data_name = "Evolution of Global Temperatures")
plot(globtemp)

# ????
time = gts_time(globtemp)
fit = gam(globtemp ~ s(time))

check(fit, simple = TRUE)

# Set seed for reproducibility
set.seed(9)

# Define sample size
n = 100

# Define beta
beta = 0.005

# Define sigma2
sigma2 = 1

# Simulation of Yt
Yt_case1 = gen_gts(WN(sigma2 = sigma2), n = n)
Yt_case2 = gen_gts(AR(phi = c(0.95, -0.5), sigma2 = sigma2), n = n)

# Define explanatory variable (time)
time = 1:n

# Simulation of Xt
Xt_case1 = beta*time + Yt_case1
Xt_case2 = beta*time + Yt_case2

# Fit a linear models
model1 <- lm(Xt_case1 ~ time + 0)
model2 <- lm(Xt_case2 ~ time + 0)

# Run these summaries to the console    
summary(model1)
summary(model2)

