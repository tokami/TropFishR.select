#'---
#' title: Simulating fisheries with specific selectivity characteristics
#' author: Tobias Mildenberger
#' date: 30/06/2017
#' output: html_document
#'---
#'
#' ## Intro to GitHub and TropFishR.select
#' There are several ways how to use this package, either you use the build tools in RStudio to build the package yourself, you source all files in the R directory or you install it from GitHub directly with (requires the devtool package):
#+ eval = FALSE
devtools::install_github("tokami/TropFishR.select")
#'
#'
#' Once the select package is installed you can load it and the main TropFishR package:
require(TropFishR)
require(TropFishR.select)
#'
#'
#' ## Introduction to stockSim and Example 1
#' 
#' The function *stockSim* allows you to simulate a fish stock with certain life history traits and simulate one to several fishing fleets with specific selectivity characteristics. Important arguments of the function which will be changed in this exercise are:
#' The mean asymptotic length (Linf)
Linf.mu = 80
#' The coefficient of variation of Linf
Linf.cv = 0.1
#' The mean growth coefficient
K.mu = 0.5
#' The coefficient of variation of K
K.cv = 0.1
#' The summer point of seasonalised VBGF
ts = 0
#' The amplitude of seasonalised VBGF
C = 0
#' The natural mortality
M = 0.7
#' The time where fishery is active (reduced to one year due to convenience in catch curve analysis, important that the simulation model is in equilibrium)
fished_t = seq(18,19,1/12)
#' The fishing effort (can be matrix for several fleets)
Etf = 500
#' The catchability coefficient (can be matrix for several fleets)
qtf = 0.001
#' The fishing mortality (or harvest rate, has to be NaN is effort and catchability should be used to estimate F)
harvest_rate = NaN
#' The gear types (so far trawl or gillnet, can be a vector for several fleets)
gear_types = "trawl"
#' The list with parameters defining selectivity curve (more information in functions gillnet and logisticSelect)
sel_list = list(mesh_size=100, mesh_size1=60,select_dist="lognormal",select_p1=3, select_p2=0.5)  # parameters from the tilapia data set
#' L50 parameter of selectivity curve
(L50 = 0.25*Linf.mu)
#' The spread of selectivity curve (L75 - L25)
(wqs = L50*0.2)
(L75 = L50 + wqs/2)


#' The model can then be run with these predefined parameters.
ex1 <- stockSim(Linf.mu = Linf.mu, 
                Linf.cv = Linf.cv,
                K.mu = K.mu,
                K.cv = K.cv,
                ts = ts,
                C = C, 
                M = M,
                Etf = Etf,
                qtf = qtf,
                harvest_rate = harvest_rate,
                gear_types = gear_types,
                sel_list = sel_list,
                L50 = L50,
                wqs = wqs, 
                fished_t = fished_t,
                progressBar = FALSE)


#' ### Fishing mortality
#' In this simulation example there is only one fleet with constant effort
plot(as.Date(ex1$fisheries$fished_years, "%Y-%m-%d"), ex1$fisheries$F, 
     type = 'l', main = "Constant fishing mortality",
     xlab = "Year", ylab = "F", lwd=2,col=4)

#' ### Selectivity characteristics
Lt = seq(0,Linf.mu,0.01)
pt1 <-  logisticSelect(Lt,L50,wqs) * 100
plot(Lt,pt1,type="l", col = 'blue',lwd=2,
     main = "Selectivity curve",
     xlab = "Length", ylab = "Probability of capture")


#' ### Catch curve analysis
#' convert data list to lfq class
class(ex1$lfqbin) <- "lfq"
#' plot LFQ data
plot(ex1$lfqbin, Fname = "catch")
#' modify lfq data and add true growth parameters
ex1mod <- lfqModify(ex1$lfqbin, 
                    par = list(Linf = Linf.mu, K = K.mu, t0 = 0), 
                    vectorise_catch = TRUE, bin_size = 2)

#' apply catch curve analysis
res1 <- catchCurve(ex1mod, catch_columns = 1, reg_int = c(10, 35), calc_ogive = TRUE)

#' compare selectiviy curves
par(mfrow=c(1,1))
plot(Lt,pt1,type="l", col = 'blue',lwd=2,
     main = "True (blue) and est. (orange) selectivity curves",
     xlab = "Length", ylab = "Probability of capture")
lines(Lt, select_ogive(list(selecType = "trawl_ogive", L50 = res1$L50, L75 = res1$L75), Lt)*100,
      col = "orange", lwd = 2)

#' resulting L50 with difference and percentage error
res1$L50
abs(res1$L50 - L50)
round(((abs(res1$L50 - L50)) / L50) * 100, 1)

#' resulting L75 with difference and percentage error
res1$L75 
abs(res1$L75 - L75)
round(((abs(res1$L75 - L75)) / L75) * 100, 1)

#' resulting wqs
(res1$L75 - res1$L50) * 2



#' ## Example 2 - Two fishing fleets
#' 
Linf.mu = 80
Linf.cv = 0.1
K.mu = 0.5
K.cv = 0.1
ts = 0
C = 0
M = 0.7
fished_t = seq(18,19,1/12)
Etf = matrix(rep(c(500,200),each=length(fished_t)),ncol=2, nrow = length(fished_t))
qtf = 0.001
harvest_rate = NaN
gear_types = c("trawl","trawl")
sel_list = list(mesh_size=100, mesh_size1=60,select_dist="lognormal",select_p1=3, select_p2=0.5)
(L50 = c(0.25*Linf.mu,0.35*Linf.mu))
(wqs = c(L50*0.2,L50*0.5))
(L75 = L50 + wqs/2)



#' The model can then be run with these predefined parameters.
ex2 <- stockSim(Linf.mu = Linf.mu, 
                Linf.cv = Linf.cv,
                K.mu = K.mu,
                K.cv = K.cv,
                ts = ts,
                C = C, 
                M = M,
                Etf = Etf,
                qtf = qtf,
                harvest_rate = harvest_rate,
                gear_types = gear_types,
                sel_list = sel_list,
                L50 = L50,
                wqs = wqs, 
                fished_t = fished_t,
                progressBar = FALSE)


#' ### Fishing mortality
plot(as.Date(ex2$fisheries$fished_years, "%Y-%m-%d"), ex2$fisheries$F[,1], 
     type = 'l', main = "Constant fishing mortality",
     xlab = "Year", ylab = "F", lwd=2, ylim = c(0,0.7), col=4)
lines(as.Date(ex2$fisheries$fished_years, "%Y-%m-%d"), 
      ex2$fisheries$F[,2], lwd=2, col="dodgerblue1")

#' ### Selectivity characteristics
Lt = seq(0,Linf.mu,0.01)
pt2a <-  logisticSelect(Lt,L50[1],wqs[1]) * 100
pt2b <-  logisticSelect(Lt,L50[2],wqs[2]) * 100
plot(Lt,pt2a,type="l", col = 'blue',lwd=2,
     main = "Selectivity curve",
     xlab = "Length", ylab = "Probability of capture")
lines(Lt, pt2b, lwd=2, col="dodgerblue1")


#' ### Catch curve analysis
#' convert data list to lfq class
class(ex2$lfqbin) <- "lfq"
#' plot LFQ data
plot(ex2$lfqbin, Fname = "catch")
#' modify lfq data and add true growth parameters
ex2mod <- lfqModify(ex2$lfqbin, 
                    par = list(Linf = Linf.mu, K = K.mu, t0 = 0), 
                    vectorise_catch = TRUE, bin_size = 2)

#' apply catch curve analysis
res2 <- catchCurve(ex2mod, catch_columns = 1, reg_int = c(11, 33), calc_ogive = TRUE)

#' compare selectiviy curves
par(mfrow=c(1,1))
plot(Lt,pt2a,type="l", col = 'blue',lwd=2,
     main = "True (blue) and est. (orange) selectivity curves",
     xlab = "Length", ylab = "Probability of capture")
lines(Lt, pt2b, lwd=2, col="dodgerblue1")
lines(Lt, select_ogive(list(selecType = "trawl_ogive", L50 = res2$L50, L75 = res2$L75), Lt)*100,
      col = "orange", lwd = 2)

#' resulting L50
res2$L50

#' resulting L75
res2$L75 

#' resulting wqs
(res2$L75 - res2$L50) * 2



#' ## Example 3 - Gillnet selectivity
#' 
Linf.mu = 80
Linf.cv = 0.1
K.mu = 0.5
K.cv = 0.1
ts = 0
C = 0
M = 0.7
fished_t = seq(18,19,1/12)
Etf = 500
qtf = 0.001
harvest_rate = NaN
gear_types = "gillnet"
sel_list = list(mesh_size=100, mesh_size1=60,select_dist="lognormal",select_p1=3, select_p2=0.5)



#' The model can then be run with these predefined parameters.
ex3 <- stockSim(Linf.mu = Linf.mu, 
                Linf.cv = Linf.cv,
                K.mu = K.mu,
                K.cv = K.cv,
                ts = ts,
                C = C, 
                M = M,
                Etf = Etf,
                qtf = qtf,
                harvest_rate = harvest_rate,
                gear_types = gear_types,
                sel_list = sel_list,
                fished_t = fished_t,
                progressBar = FALSE)

#' ### Fishing mortality
plot(as.Date(ex3$fisheries$fished_years, "%Y-%m-%d"), ex3$fisheries$F, 
     type = 'l', main = "Constant fishing mortality",
     xlab = "Year", ylab = "F", lwd=2, col=4)

#' ### Selectivity characteristics
Lt = seq(0,Linf.mu,0.01)
pt3 <- do.call(TropFishR.select::gillnet,c(sel_list,list(Lt=Lt))) * 100
plot(Lt,pt3,type="l", col = 'blue',lwd=2,
     main = "Selectivity curve",
     xlab = "Length", ylab = "Probability of capture")


#' ### Catch curve analysis
#' convert data list to lfq class
class(ex3$lfqbin) <- "lfq"
#' plot LFQ data
plot(ex3$lfqbin, Fname = "catch")
#' modify lfq data and add true growth parameters
ex3mod <- lfqModify(ex3$lfqbin, 
                    par = list(Linf = Linf.mu, K = K.mu, t0 = 0), 
                    vectorise_catch = TRUE, bin_size = 2)

#' apply catch curve analysis
res3 <- catchCurve(ex3mod, catch_columns = 1, reg_int = c(8, 36), calc_ogive = TRUE)

#' compare selectiviy curves
par(mfrow=c(1,1))
plot(Lt,pt3,type="l", col = 'blue',lwd=2,
     main = "True (blue) and est. (orange) selectivity curves",
     xlab = "Length", ylab = "Probability of capture")
lines(Lt, select_ogive(list(selecType = "trawl_ogive", L50 = res2$L50, L75 = res2$L75), Lt)*100,
      col = "orange", lwd = 2)

#' resulting L50
res2$L50

#' resulting L75
res2$L75 

#' resulting wqs
(res2$L75 - res2$L50) * 2
