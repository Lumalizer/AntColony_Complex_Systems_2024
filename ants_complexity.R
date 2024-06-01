library(randtests)
library(mgcv, lib.loc ="C:/Program Files/R/R-4.4.0/library")
library(ecp)
library(tseries)
library(dplyr)
library(changepoint)
library(changepoint.np)
library(mvpart)
library(ggplot2)
library(rEDM)
library(knitr)
library(kableExtra)

#COLONY 1 ANT 11
ant11 <- preprocessed_ant_data[preprocessed_ant_data$ant_id == 11, ]

means <- c(mean(ant11$x), mean(ant11$y), mean(ant11$angle), mean(ant11$speed))
sds <- c(sd(ant11$x), sd(ant11$y), sd(ant11$angle), sd(ant11$speed))

variables <- c('x', 'y', 'Angle', 'Speed')
means <-round(means, 2)
sds <-round(sds, 2)

#-------------------------------------------------------------------------------
#COLONY 6 ANT 4
anto <- ant_data_outdoor[ant_data_outdoor$ant_id== 4,]

meanso <- c(mean(anto$x), mean(anto$y), mean(na.exclude(anto$angle)), mean(na.exclude(anto$speed)))
sdso <- c(sd(anto$x), sd(anto$y),sd(na.exclude(anto$angle)), sd(na.exclude(anto$speed)))

variableso <- c('x', 'y', 'Angle', 'Speed')
meanso <-round(meanso, 2)
sdso <-round(sdso, 2)

#memory 
#1. dependency on past values -> bartels rank test###########################################

#X
bartels.rank.test(ant11$x, alternative = "two.sided")

#Y
bartels.rank.test(ant11$y, alternative = "two.sided")

#ANGLE
bartels.rank.test(ant11$angle, alternative = "two.sided")

#SPEED
bartels.rank.test(ant11$speed, alternative = "two.sided"
                  

baretels_results <-c('<.001*', '<.001*', '<.05*', '<.001*')
#-------------------------------------------------------------------------------

#X
bartels.rank.test(anto$x, alternative = "two.sided")

#Y
bartels.rank.test(anto$y, alternative = "two.sided")

#ANGLE
bartels.rank.test(anto$angle, alternative = "two.sided")

#SPEED
bartels.rank.test(anto$speed, alternative = "two.sided")
                  
baretels_resultso <-c('<.001*', '<.001*', '.05*', '<.05*')

#2.longe-range temporal correlations -> inspect (partial) autocorrelation functions#############

#x

pacf_resultsx <- pacf(na.exclude(ant11$x), lag.max = 1000, plot = FALSE)
N <- length(na.exclude(ant11$x))
significant_lags <- length(which(abs(pacf_resultsx$acf) > (2 / sqrt(N))))
max_significant_lag <- max(which(abs(pacf_resultsx$acf) > (2 / sqrt(N))))

#y

pacf_resultsy <- pacf(na.exclude(ant11$y), lag.max = 1000, plot = FALSE)
N <- length(na.exclude(ant11$y))
significant_lags <- length(which(abs(pacf_resultsy$acf) > (2 / sqrt(N))))
max_significant_lag <- max(which(abs(pacf_resultsy$acf) > (2 / sqrt(N))))

#angle

pacf_resultsa <- pacf(na.exclude(ant11$angle), lag.max = 1000, plot = FALSE)
N <- length(na.exclude(ant11$angle))
significant_lags <- length(which(abs(pacf_resultsa$acf) > (2 / sqrt(N))))
max_significant_lag <- max(which(abs(pacf_resultsa$acf) > (2 / sqrt(N))))

#speed

pacf_resultss <- pacf(na.exclude(ant11$speed), lag.max = 1000, plot = FALSE)
N <- length(na.exclude(ant11$speed))
significant_lags <- length(which(abs(pacf_resultss$acf) > (2 / sqrt(N))))
max_significant_lag <- max(which(abs(pacf_resultss$acf) > (2 / sqrt(N))))

################################################

par(mfrow=c(2,2))

plot(pacf_resultsx, main = "x", ylab = "Partial Autocorrelation")
plot(pacf_resultsy, main = "y", ylab = "Partial Autocorrelation")
plot(pacf_resultsa, main = "Angle", ylab = "Partial Autocorrelation")
plot(pacf_resultss, main = "Speed", ylab = "Partial Autocorrelation")
mtext("Partial Autocorrelation (indoor ant)", side = 3, outer = TRUE, line = -1, cex = 1.5)

par(mfrow=c(1,1))

ns <- c('9', '1', '3', '9')
maxs <-c('23', '1', '96', '93')
#-------------------------------------------------------------------------------

#x

pacf_resultsx <- pacf(na.exclude(anto$x), lag.max = 1000, plot = FALSE)
N <- length(na.exclude(anto$x))
significant_lags <- length(which(abs(pacf_resultsx$acf) > (2 / sqrt(N))))
max_significant_lag <- max(which(abs(pacf_resultsx$acf) > (2 / sqrt(N))))

#y

pacf_resultsy <- pacf(na.exclude(anto$y), lag.max = 1000, plot = FALSE)
N <- length(na.exclude(anto$y))
significant_lags <- length(which(abs(pacf_resultsy$acf) > (2 / sqrt(N))))
max_significant_lag <- max(which(abs(pacf_resultsy$acf) > (2 / sqrt(N))))

#angle

pacf_resultsa <- pacf(na.exclude(anto$angle), lag.max = 1000, plot = FALSE)
N <- length(na.exclude(anto$angle))
significant_lags <- length(which(abs(pacf_resultsa$acf) > (2 / sqrt(N))))
max_significant_lag <- max(which(abs(pacf_resultsa$acf) > (2 / sqrt(N))))

#speed

pacf_resultss <- pacf(na.exclude(anto$speed), lag.max = 1000, plot = FALSE)
N <- length(na.exclude(anto$speed))
significant_lags <- length(which(abs(pacf_resultss$acf) > (2 / sqrt(N))))
max_significant_lag <- max(which(abs(pacf_resultss$acf) > (2 / sqrt(N))))

################################################

par(mfrow=c(2,2))

plot(pacf_resultsx, main = "x", ylab = "Partial Autocorrelation")
plot(pacf_resultsy, main = "y", ylab = "Partial Autocorrelation")
plot(pacf_resultsa, main = "Angle", ylab = "Partial Autocorrelation")
plot(pacf_resultss, main = "Speed", ylab = "Partial Autocorrelation")
mtext("Partial Autocorrelaé&tion (outdoor ant)", side = 3, outer = TRUE, line = -1, cex = 1.5)

par(mfrow=c(1,1))

nso <- c('4', '1', '5', '6')
maxso <-c('463', '1', '183', '56')
#non-stationarity temporal correlations -> time-varying autoregressive model####

tt <-1:(nrow(ant11)-1)
tvar = gam(ant11$x[2:339] ~ s(tt, by= ant11$x[1:338], k = 10, bs = 'tp'))
stvar <- summary(tvar)
plot.gam(tvar, select = 1, ,rug = FALSE, seWithMean = TRUE, ylab = 'Autoregressive parameter', xlab= 'Day')
round(round(stvar$edf, 3))

#Y
tt <-1:(nrow(ant11)-1)
tvar = gam(ant11$y[2:339] ~ s(tt, by= ant11$y[1:338], k = 10, bs = 'tp'))
stvar <- summary(tvar)
plot.gam(tvar, select = 1, ,rug = FALSE, seWithMean = TRUE, ylab = 'Autoregressive parameter', xlab= 'Day')
round(round(stvar$edf, 3))

#angle
tt <-1:(nrow(ant11)-1)
tvar = gam(ant11$angle[2:339] ~ s(tt, by= ant11$angle[1:338], k = 10, bs = 'tp'))
stvar <- summary(tvar)
plot.gam(tvar, select = 1, ,rug = FALSE, seWithMean = TRUE, ylab = 'Autoregressive parameter', xlab= 'Day')
round(round(stvar$edf, 3))

#speed
tt <-1:(nrow(ant11)-1)
tvar = gam(ant11$speed[2:339] ~ s(tt, by= ant11$speed[1:338], k = 10, bs = 'tp'))
stvar <- summary(tvar)
plot.gam(tvar, select = 1, ,rug = FALSE, seWithMean = TRUE, ylab = 'Autoregressive parameter', xlab= 'Day')
round(round(stvar$edf, 3))

DF <-c('10*', '9*', '6*', '10*' )

#-------------------------------------------------------------------------------

tt <-1:(nrow(anto)-1)
tvar = gam(anto$x[2:677] ~ s(tt, by= anto$x[1:676], k = 10, bs = 'tp'))
stvar <- summary(tvar)
plot.gam(tvar, select = 1, ,rug = FALSE, seWithMean = TRUE, ylab = 'Autoregressive parameter', xlab= 'Day')
round(round(stvar$edf, 3))

#Y
tt <-1:(nrow(anto)-1)
tvar = gam(anto$y[2:677] ~ s(tt, by= anto$y[1:676], k = 10, bs = 'tp'))
stvar <- summary(tvar)
plot.gam(tvar, select = 1, ,rug = FALSE, seWithMean = TRUE, ylab = 'Autoregressive parameter', xlab= 'Day')
round(round(stvar$edf, 3))

#angle
tt <-1:(nrow(anto)-1)
tvar = gam(anto$angle[2:677] ~ s(tt, by= anto$angle[1:676], k = 10, bs = 'tp'))
stvar <- summary(tvar)
plot.gam(tvar, select = 1, ,rug = FALSE, seWithMean = TRUE, ylab = 'Autoregressive parameter', xlab= 'Day')
round(round(stvar$edf, 3))

#speed
tt <-1:(nrow(anto)-1)
tvar = gam(anto$speed[2:677] ~ s(tt, by= anto$speed[1:676], k = 10, bs = 'tp'))
stvar <- summary(tvar)
plot.gam(tvar, select = 1, ,rug = FALSE, seWithMean = TRUE, ylab = 'Autoregressive parameter', xlab= 'Day')
round(round(stvar$edf, 3))

DFO <-c('9*', '10*', '2*', '10*' )

################################################################################
#regime shifts, non-stationarity
#1.KPSS

#X
kpss.test(na.exclude(ant11$x),lshort=TRUE,null="Level")

#Y
kpss.test(na.exclude(ant11$y),lshort=TRUE,null="Level")

#ANGLE
kpss.test(na.exclude(ant11$angle),lshort=TRUE,null="Level")

#SPEED
kpss.test(na.exclude(ant11$speed),lshort=TRUE,null="Level")


KPSS <- c('0.01*', '0.01*','0.1', '0.06' )

#-------------------------------------------------------------------------------

#X
kpss.test(na.exclude(anto$x),lshort=TRUE,null="Level")

#Y
kpss.test(na.exclude(anto$y),lshort=TRUE,null="Level")

#ANGLE
kpss.test(na.exclude(anto$angle),lshort=TRUE,null="Level")

#SPEED
kpss.test(na.exclude(anto$speed),lshort=TRUE,null="Level")


KPSSO <- c('0.01*', '0.01*','0.8', '0.1' )

#2.CHANGE POINT DETECTION#######################################################

#X
tsx <- matrix(na.exclude(ant11$x)) 
e.out <- e.divisive(tsx, R=500, sig.lvl=.002) 
df.e <- length(which(e.out$p.values<.002))
e.out$estimates
change_pointsx <- e.out$estimates

#y
tsy <- matrix(na.exclude(ant11$y)) 
e.out <- e.divisive(tsy, R=500, sig.lvl=.002) 
df.e <- length(which(e.out$p.values<.002))
e.out$estimates
change_pointsy <- e.out$estimates

#angle
tsa <- matrix(na.exclude(ant11$angle)) 
e.out <- e.divisive(tsa, R=500, sig.lvl=.002) 
df.e <- length(which(e.out$p.values<.002))
e.out$estimates
change_pointsa <- e.out$estimates

#speed
tss <- matrix(na.exclude(ant11$speed)) 
e.out <- e.divisive(tss, R=500, sig.lvl=.002) 
df.e <- length(which(e.out$p.values<.002))
e.out$estimates
change_pointss <- e.out$estimates


#####################################################
par(mfrow=c(2,2))

plot(tsx, type = "l", xlab = "Time", ylab = "x")
abline(v = change_pointsx, col = "red", lwd = 2, lty = 2) 
plot(tsy, type = "l", xlab = "Time", ylab = "y")
abline(v = change_pointsy, col = "red", lwd = 2, lty = 2) 
plot(tsa, type = "l", xlab = "Time", ylab = "Angle")
abline(v = change_pointsa, col = "red", lwd = 2, lty = 2) 
plot(tss, type = "l", xlab = "Time", ylab = "Speed")
abline(v = change_pointss, col = "red", lwd = 2, lty = 2) 

mtext("Phase Transitions in Time Series (indoor ant)", side = 3, outer = TRUE, line = -1, cex = 1.5)
par(mfrow=c(2,2), oma=c(0, 0, 10, 0))



n_change_points <- c('10', '10', '2', '5' )

#-------------------------------------------------------------------------------


#X
tsx <- matrix(na.exclude(anto$x)) 
e.out <- e.divisive(tsx, R=500, sig.lvl=.002) 
df.e <- length(which(e.out$p.values<.002))
e.out$estimates
change_pointsx <- e.out$estimates

#y
tsy <- matrix(na.exclude(anto$y)) 
e.out <- e.divisive(tsy, R=500, sig.lvl=.002) 
df.e <- length(which(e.out$p.values<.002))
e.out$estimates
change_pointsy <- e.out$estimates

#angle
tsa <- matrix(na.exclude(anto$angle)) 
e.out <- e.divisive(tsa, R=500, sig.lvl=.002) 
df.e <- length(which(e.out$p.values<.002))
e.out$estimates
change_pointsa <- e.out$estimates

#speed
tss <- matrix(na.exclude(anto$speed)) 
e.out <- e.divisive(tss, R=500, sig.lvl=.002) 
df.e <- length(which(e.out$p.values<.002))
e.out$estimates
change_pointss <- e.out$estimates


#####################################################
par(mfrow=c(2,2))

plot(tsx, type = "l", xlab = "Time", ylab = "x")
abline(v = change_pointsx, col = "red", lwd = 2, lty = 2) 
plot(tsy, type = "l", xlab = "Time", ylab = "y")
abline(v = change_pointsy, col = "red", lwd = 2, lty = 2) 
plot(tsa, type = "l", xlab = "Time", ylab = "Angle")
abline(v = change_pointsa, col = "red", lwd = 2, lty = 2) 
plot(tss, type = "l", xlab = "Time", ylab = "Speed")
abline(v = change_pointss, col = "red", lwd = 2, lty = 2) 

mtext("Phase Transitions in Time Series (Outdoor Ant)", side = 3, outer = TRUE, line = -1, cex = 1.5)
par(mfrow=c(2,2), oma=c(0, 0, 10, 0))



n_change_pointso <- c('13', '17', '2', '6' )

################################################################################

data <-data.frame('Item' = variables, 'Mean' = means, 'SD' = sds, 'Baretels Rank Test' = baretels_results, 'Significant Lags' = ns, 'Max Lag' = maxs, 'EDF' = DF, 'KPSS' = KPSS, 'Number of Significant Change Points' = n_change_points, check.names = FALSE) 

# Generate the table with kable
table_output <- kable(data, format = "html", booktabs = TRUE, caption = "Descriptive statistics and results of the analysis (indoor ants)")

# Enhance the table with kableExtra for better styling
table_output <- table_output %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(2, width = "50px") %>%
  scroll_box(width = "100%", height = "500px")

# Print the table
print(table_output)

#-------------------------------------------------------------------------------
data <-data.frame('Item' = variableso, 'Mean' = meanso, 'SD' = sdso, 'Baretels Rank Test' = baretels_resultso, 'Significant Lags' = nso, 'Max Lag' = maxso, 'EDF' = DFO, 'KPSS' = KPSSO, 'Number of Significant Change Points' = n_change_pointso, check.names = FALSE) 

# Generate the table with kable
table_output <- kable(data, format = "html", booktabs = TRUE, caption = "Descriptive statistics and results of the analysis (Outdoor Ant)")

# Enhance the table with kableExtra for better styling
table_output <- table_output %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(2, width = "50px") %>%
  scroll_box(width = "100%", height = "500px")

# Print the table
print(table_output)


#########################################################################################

#sensitive dependence on initial conditions
#limited predictive horizon -> forecast skill

X <- as.data.frame(cbind(1:399,ant11$x))
names(X) <- c("frameno","x")
X$x <- as.numeric(scale(X$x))

rho_emd <-EmbedDimension(dataFrame =X, lib = "1 169", pred = "170 399", columns='x', target='x')
simplex_out <- Simplex(dataFrame = X, lib = "1 169", pred = "170 399", E=1,  tau=5,columns='x', target ='x')
       
plot(simplex_out$Observations[2:170],type='l', xlab = "Time", ylab="Value", main ="x")
lines(simplex_out$Predictions[2:170],type='l',col=2,lty=2)
       
# plotting how mse changes with time
observations <- simplex_out$Observations[2:170]
predictions <- simplex_out$Predictions[2:170]
squared_errorsx <- (observations - predictions)^2

#y
Y <- as.data.frame(cbind(1:399,ant11$y))
names(Y) <- c("frameno","y")
Y$y <- as.numeric(scale(Y$y))

rho_emd <-EmbedDimension(dataFrame =Y, lib = "1 169", pred = "170 399", columns='y', target='y')
simplex_out <- Simplex(dataFrame = Y, lib = "1 169", pred = "170 399", E=1,  tau=5,columns='y', target ='y')

plot(simplex_out$Observations[2:170],type='l', xlab = "Time", ylab="Value", main ="y")
lines(simplex_out$Predictions[2:170],type='l',col=2,lty=2)
       
# plotting how mse changes with time
observations <- simplex_out$Observations[2:170]
predictions <- simplex_out$Predictions[2:170]
squared_errorsy <- (observations - predictions)^2

#angle
ANGLE <- as.data.frame(cbind(1:399,ant11$angle))
names(ANGLE) <- c("frameno","angle")
ANGLE$angle <- as.numeric(scale(ANGLE$angle))
    
rho_emd <-EmbedDimension(dataFrame =ANGLE, lib = "1 169", pred = "170 399", columns='angle', target='angle')
simplex_out <- Simplex(dataFrame = ANGLE, lib = "1 169", pred = "170 399", E=7, tau=5, columns='angle', target ='angle')
       
plot(simplex_out$Observations[2:170],type='l', xlab = "Time", ylab="Value", main ="angle")
lines(simplex_out$Predictions[2:170],type='l',col=2,lty=2)

# plotting how mse changes with time
observations <- simplex_out$Observations[2:170]
predictions <- simplex_out$Predictions[2:170]
squared_errorsa <- (observations - predictions)^2

#speed
SPEED <- as.data.frame(cbind(1:399,ant11$speed))
names(SPEED) <- c("frameno","speed")
SPEED$speed <- as.numeric(scale(SPEED$speed))


rho_emd <-EmbedDimension(dataFrame =SPEED, lib = "1 169", pred = "170 399", columns='speed', target='speed')
simplex_out <- Simplex(dataFrame = SPEED, lib = "1 169", pred = "170 399", E=7, tau=5, columns='speed', target ='speed')
       
plot(simplex_out$Observations[2:170],type='l', xlab = "Time", ylab="Value", main ="speed")
lines(simplex_out$Predictions[2:170],type='l',col=2,lty=2)

# plotting how mse changes with time
observations <- simplex_out$Observations[2:170]
predictions <- simplex_out$Predictions[2:170]
squared_errorss <- (observations - predictions)^2

par(mfrow=c(2,2))
plot(squared_errorsx, type = 'l', col = 'blue', xlab = "Time Step", ylab = "Squared Error",main = "x")
plot(squared_errorsy, type = 'l', col = 'blue', xlab = "Time Step", ylab = "Squared Error", main = "y" )
plot(squared_errorsa, type = 'l', col = 'blue', xlab = "Time Step", ylab = "Squared Error",main = "Angle")
plot(squared_errorss, type = 'l', col = 'blue', xlab = "Time Step", ylab = "Squared Error",main = "Speed")
mtext("Squared Errors for Each Prediction Point (Indoor Ant)", side = 3, outer = TRUE, line = -1, cex = 1.5)
par(mfrow=c(2,2), oma=c(0, 0, 20, 0))
#-------------------------------------------------------------------------------
X <- as.data.frame(cbind(1:677,anto$x))
names(X) <- c("frameno","x")
X$x <- as.numeric(scale(X$x))

rho_emd <-EmbedDimension(dataFrame =X, lib = "1 338", pred = "339 677", columns='x', target='x')
simplex_out <- Simplex(dataFrame = X, lib = "1 338", pred = "339 677", E=1, tau=5,columns='x', target ='x')

plot(simplex_out$Observations[2:336],type='l', xlab = "Time", ylab="Value", main ="x")
lines(simplex_out$Predictions[2:336],type='l',col=2,lty=2)

# plotting how mse changes with time
observations <- simplex_out$Observations[2:336]
predictions <- simplex_out$Predictions[2:336]
squared_errorsx <- (observations - predictions)^2

#y
Y <- as.data.frame(cbind(1:677,anto$y))
names(Y) <- c("frameno","y")
Y$y <- as.numeric(scale(Y$y))

rho_emd <-EmbedDimension(dataFrame =Y, lib = "1 336", pred = "339 677", columns='y', target='y')
simplex_out <- Simplex(dataFrame = Y, lib = "1 336", pred = "339 677", E=1,tau=5, columns='y', target ='y')

plot(simplex_out$Observations[2:336],type='l', xlab = "Time", ylab="Value", main ="y")
lines(simplex_out$Predictions[2:336],type='l',col=2,lty=2)

# plotting how mse changes with time
observations <- simplex_out$Observations[2:336]
predictions <- simplex_out$Predictions[2:336]
squared_errorsy <- (observations - predictions)^2


#angle
ANGLE <- as.data.frame(cbind(1:625,anto$angle))
names(ANGLE) <- c("frameno","angle")
ANGLE <- na.exclude(ANGLE)
ANGLE$angle <- as.numeric(scale(ANGLE$angle))

rho_emd <-EmbedDimension(dataFrame =ANGLE, lib = "1 338", pred = "339 625", columns='angle', target='angle')
simplex_out <- Simplex(dataFrame = ANGLE, lib = "1 338", pred = "339 625", E=1,tau=5, columns='angle', target ='angle')

plot(simplex_out$Observations[2:280],type='l', xlab = "Time", ylab="Value", main ="angle")
lines(simplex_out$Predictions[2:280],type='l',col=2,lty=2)

# plotting how mse changes with time
observations <- simplex_out$Observations[2:280]
predictions <- simplex_out$Predictions[2:280]
squared_errorsa <- (observations - predictions)^2

#speed
SPEED <- as.data.frame(cbind(1:677,anto$speed))
names(SPEED) <- c("frameno","speed")
SPEED <- na.exclude(SPEED)
SPEED$speed <- as.numeric(scale(SPEED$speed))

rho_emd <-EmbedDimension(dataFrame =SPEED, lib = "1 338", pred = "399 676", columns='speed', target='speed')
simplex_out <- Simplex(dataFrame = SPEED, lib = "1 338", pred = "399 676", E=3, tau=5,columns='speed', target ='speed')

plot(simplex_out$Observations[2:280],type='l', xlab = "Time", ylab="Value", main ="speed")
lines(simplex_out$Predictions[2:280],type='l',col=2,lty=2)

# plotting how mse changes with time
observations <- simplex_out$Observations[2:280]
predictions <- simplex_out$Predictions[2:280]
squared_errors <- (observations - predictions)^2

par(mfrow=c(2,2))
plot(squared_errorsx, type = 'l', col = 'blue', xlab = "Time Step", ylab = "Squared Error",main = "x")
plot(squared_errorsy, type = 'l', col = 'blue', xlab = "Time Step", ylab = "Squared Error",main = "y")
plot(squared_errorsa, type = 'l', col = 'blue', xlab = "Time Step", ylab = "Squared Error", main = "Angle")
plot(squared_errorss, type = 'l', col = 'blue', xlab = "Time Step", ylab = "Squared Error", main = "Speed")
mtext("Squared Errors for Each Prediction Point (Outdoor Ant)", side = 3, outer = TRUE, line = -1, cex = 1.5)
par(mfrow=c(2,2), oma=c(0, 0, 10, 0))