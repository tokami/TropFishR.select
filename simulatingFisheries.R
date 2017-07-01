#'---
#' title: Simulating fisheries with specific selectivity characteristics
#' author: Tobias Mildenberger
#' date: 30/06/2017
#' export: html_document
#'---
#'
#' Load packages
require(TropFishR)
require(TropFishR.select)
#'
#' ## Introduction to stockSim.R
#' 
#' The function *stockSim* allows you to simulate a fish stock with certain life history traits and simulate one to several fishing fleets with specific selectivity characteristics. Important arguments of the function which will be changed in this exercise are:
#' mean asymptotic length (Linf)
Linf.mu = 80
#' coefficient of variation of Linf
Linf.cv = 0.1
#' mean growth coefficient
K.mu = 0.5
#' coefficient of variation of K
K.cv = 0.1
#' summer point of seasonalised VBGF
ts = 0
#' amplitude of seasonalised VBGF
C = 0
#' natural mortality
M = 0.7
#' fishing effort
Etf = 500
#' catchability coefficient
qtf = 0.001
#' fishing mortality (or harvest rate)
harvest_rate = NaN
#' gear types (so far trawl or gillnet)
gear_types = "trawl"
#' list with parameters defining selectivity curve (more information in functions gillnet and logisticSelect)
sel_list = list(mesh_size=100, mesh_size1=60,select_dist="lognormal",select_p1=3, select_p2=0.5)  # parameters from the tilapia data set
#' L50 parameter of selectivity curve
(L50 = 0.25*Linf.mu)
#' spread of selectivity curve (L75 - L25)
(wqs = L50*0.2)
(L75 = L50 + wqs/2)
#' time where fishery is active
fished_t = seq(18,19,1/12)


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
plot(as.Date(ex1$fisheries$fished_years, "%Y-%m-%d"), ex1$fisheries$F, 
     type = 'l', main = "Constant fishing mortality",
     xlab = "Year", ylab = "F", lwd=2)

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
