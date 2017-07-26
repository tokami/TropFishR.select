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
set.seed(200)
#'
#' ## Trawl fleet only
#' 
#' The function *stockSim* allows you to simulate a fish stock with certain life history traits and simulate one to several fishing fleets with specific selectivity characteristics. Important arguments of the function which will be changed in this exercise are:
#' The mean asymptotic length (Linf)
Linf.mu = 93.5
#' The coefficient of variation of Linf
Linf.cv = 0.1
#' The mean growth coefficient
K.mu = 0.2
#' The coefficient of variation of K
K.cv = 0.1
#' The summer point of seasonalised VBGF
ts = 0

t0 = 0.28
#' The amplitude of seasonalised VBGF
C = 0
#' The natural mortality
M = 0.4
#' The time where fishery is active (reduced to one year due to convenience in catch curve analysis, important that the simulation model is in equilibrium)
fished_t = seq(0,35,1/12)
timemax = 35
#' The fishing effort (can be matrix for several fleets)
Etf = 500
#' The catchability coefficient (can be matrix for several fleets)
qtf = 0.001
#' The fishing mortality (or harvest rate, has to be NaN is effort and catchability should be used to estimate F)
harvest_rate = 0.76 ## c(0.5, 0.26) ##
#' The gear types (so far trawl or gillnet, can be a vector for several fleets)
gear_types = c("trawl")
#' The list with parameters defining selectivity curve (more information in functions gillnet and logisticSelect)
sel_list = NULL ## list(mesh_size=18.6, mesh_size1=60,select_dist="gamma",select_p1=3, select_p2=0.5)  # parameters from the tilapia data set
#' L50 parameter of trawl-like selectivity curve
L50 = 55
#' The spread of trawl-like selectivity curve (L75 - L25)
wqs = 7.2 * 2
#' L75 parameter of trawl-like selectivity curve
(L75 = L50 + wqs/2)
#' The length at first maturity
Lmat = 63.4

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
                timemax = timemax,
                Lmat = Lmat,
                progressBar = FALSE)


#' ### Fishing mortality
#' In this simulation example there is only one fleet with constant effort
plot(as.Date(ex1$fisheries$fished_years, "%Y-%m-%d"), ex1$fisheries$F, 
     type = 'l', main = "Constant fishing mortality",
     xlab = "Year", ylab = "F", lwd=2,col=4)

#' ### Selectivity characteristics
Lt = seq(0,Linf.mu+20,0.01)
pt1 <-  logisticSelect(Lt,L50,wqs) * 100
plot(Lt,pt1,type="l", col = 'blue',lwd=2,
     main = "Selectivity curve",
     xlab = "Length", ylab = "Probability of capture")


#' ### Catch curve analysis
lfqdat <- ex1$lfqbin
#' subsample data for 2014
lfqdat$catch <- lfqdat$catch[,which(format(lfqdat$dates, "%Y") %in% "2014")]
lfqdat$dates <- lfqdat$dates[format(lfqdat$dates, "%Y") %in% "2014"]
#' convert data list to lfq class
class(lfqdat) <- "lfq"
#' plot LFQ data
plot(lfqdat, Fname = "catch")
#' modify lfq data and add true growth parameters
ex1mod <- lfqModify(lfqdat, 
                    par = list(Linf = Linf.mu, K = K.mu, t0 = t0), 
                    vectorise_catch = TRUE, bin_size = 4)

#' apply catch curve analysis
res1 <- catchCurve(ex1mod, reg_int = c(14, 20), calc_ogive = TRUE)

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



#' ## All fleets (trawl + gillnet)
#' 
Linf.mu = 93.5
Linf.cv = 0.1
K.mu = 0.2
K.cv = 0.1
t0 = 0.28
ts = 0
C = 0
M = 0.4
fished_t = seq(0,35,1/12)
timemax = 35
# Etf = matrix(rep(c(500,200),each=length(fished_t)),ncol=2, nrow = length(fished_t))
# qtf = 0.001
harvest_rate = c(0.5,0.2/3,0.2/3,0.2/3)
gear_types = c("trawl","gillnet","gillnet","gillnet")
sel_list1 <- list(mesh_size=1, mesh_size1=NULL, select_dist="normal_fixed", select_p1=94.7, select_p2=13.67)
sel_list2 <- list(mesh_size=1, mesh_size1=NULL, select_dist="normal_fixed", select_p1=101.8, select_p2=14.7)
sel_list3 <- list(mesh_size=1, mesh_size1=NULL, select_dist="normal_fixed", select_p1=112, select_p2=16.17)
sel_list = list(NULL,sel_list1, sel_list2, sel_list3)
#' L50 parameter of trawl-like selectivity curve
L50 = 55
#' The spread of trawl-like selectivity curve (L75 - L25)
wqs = 7.2 * 2
#' L75 parameter of trawl-like selectivity curve
(L75 = L50 + wqs/2)
#' The length at first maturity
Lmat = 63.4



#' The model can then be run with these predefined parameters.
ex2 <- stockSim(Linf.mu = Linf.mu, 
                Linf.cv = Linf.cv,
                K.mu = K.mu,
                K.cv = K.cv,
                ts = ts,
                C = C, 
                M = M,
                harvest_rate = harvest_rate,
                gear_types = gear_types,
                sel_list = sel_list,
                L50 = L50,
                wqs = wqs, 
                fished_t = fished_t,
                timemax = timemax,
                progressBar = FALSE)


#' ### Fishing mortality
plot(as.Date(ex2$fisheries$fished_years, "%Y-%m-%d"), ex2$fisheries$F[,1], 
     type = 'l', main = "Constant fishing mortality",
     xlab = "Year", ylab = "F", lwd=2, ylim = c(0,0.7), col=4)
lines(as.Date(ex2$fisheries$fished_years, "%Y-%m-%d"), 
      ex2$fisheries$F[,2], lwd=2, col="dodgerblue1")

#' ### Selectivity characteristics
#' 


# sel_list1 <- list(mesh_size=186, mesh_size1=60, select_dist="lognormal", select_p1=3.7, select_p2=0.5)
# sel_list2 <- list(mesh_size=200, mesh_size1=60, select_dist="lognormal", select_p1=3.7, select_p2=0.5)
# sel_list3 <- list(mesh_size=220, mesh_size1=60, select_dist="lognormal", select_p1=3.7, select_p2=0.5)

sel_list1 <- list(mesh_size=1, mesh_size1=NULL, select_dist="normal_fixed", select_p1=94.7, select_p2=13.67)
sel_list2 <- list(mesh_size=1, mesh_size1=NULL, select_dist="normal_fixed", select_p1=101.8, select_p2=14.7)
sel_list3 <- list(mesh_size=1, mesh_size1=NULL, select_dist="normal_fixed", select_p1=112, select_p2=16.17)


Lt = seq(0,Linf.mu+40,0.01)
pt1 <-  logisticSelect(Lt,L50,wqs) * 100
pt2a <-  do.call(TropFishR.select::gillnet,c(sel_list1,list(Lt=Lt))) * 100
pt2b <-  do.call(TropFishR.select::gillnet,c(sel_list2,list(Lt=Lt))) * 100
pt2c <-  do.call(TropFishR.select::gillnet,c(sel_list3,list(Lt=Lt))) * 100
plot(Lt,pt1,type="l", col = 'blue',lwd=2,
     main = "Selectivity curve",
     xlab = "Length", ylab = "Probability of capture")
lines(Lt, pt2a, lwd=2, col="dodgerblue4")
lines(Lt, pt2b, lwd=2, col="dodgerblue3")
lines(Lt, pt2c, lwd=2, col="dodgerblue1")

abline(v=94.7, col='grey',lwd=2,lty=2)
abline(v=101.8, col='grey',lwd=2,lty=2)
abline(v=112, col='grey',lwd=2,lty=2)


#' ### Catch curve analysis
lfqdat <- ex2$lfqbin
#' subsample data for 2014
lfqdat$catch <- lfqdat$catch[,which(format(lfqdat$dates, "%Y") %in% "2014")]
lfqdat$dates <- lfqdat$dates[format(lfqdat$dates, "%Y") %in% "2014"]
#' convert data list to lfq class
class(lfqdat) <- "lfq"
#' plot LFQ data
plot(lfqdat, Fname = "catch")
#' modify lfq data and add true growth parameters
ex2mod <- lfqModify(lfqdat, 
                    par = list(Linf = Linf.mu, K = K.mu, t0 = t0), 
                    vectorise_catch = TRUE, bin_size = 4)

#' apply catch curve analysis
res2 <- catchCurve(ex2mod, reg_int = c(12, 19), calc_ogive = TRUE)

#' compare selectiviy curves
par(mfrow=c(1,1))
plot(Lt,pt1,type="l", col = 'blue',lwd=2,
     main = "True (blue) and est. (orange) selectivity curves",
     xlab = "Length", ylab = "Probability of capture")
lines(Lt, pt2a, lwd=2, col="dodgerblue4")
lines(Lt, pt2b, lwd=2, col="dodgerblue3")
lines(Lt, pt2c, lwd=2, col="dodgerblue1")
lines(Lt, select_ogive(list(selecType = "trawl_ogive", L50 = res2$L50, L75 = res2$L75), Lt)*100,
      col = "orange", lwd = 2)

#' I define effective selectivity curves here as the combined selectivity curve of multiple gears with different selectivity curves in a multi-fleet multi-gear sceanrio. There might be a proper definition of this selectivity, but I don't have time to search the web for this term. 
ptAll <- (pt1/100 * 0.5 + pt2a/100 * (0.2/3) + pt2b/100 * (0.2/3) + pt2c/100 * (0.2/3)) 
plot(Lt,ptAll, type="n", col = 'blue',lwd=2, ylim = c(0,0.8),
     xlab = "Length", ylab = "Fishing mortality per length per gear")
lines(Lt, select_ogive(list(selecType = "trawl_ogive", L50 = res2$L50, L75 = res2$L75), Lt)*0.7,
      col = "orange", lwd = 2)
lines(Lt,ptAll, col = 'blue',lwd=2)
## draw selectivities from single gears
lines(Lt, pt1/100* 0.5, lwd=2, col="grey",lty=2)
lines(Lt, pt2a/100 * (0.2/3), lwd=2, col="grey",lty=2)
lines(Lt, pt2b/100 * (0.2/3), lwd=2, col="grey",lty=2)
lines(Lt, pt2c/100 * (0.2/3), lwd=2, col="grey",lty=2)
legend("topleft", legend = c('estimated','cumulative','single'), 
       col=c('orange','blue','grey'),lty=c(1,1,2), bty='n', 
       cex = 0.8, y.intersp = 0.8, seg.len = 0.7)

#' Now, the difficulty is to evaluate the difference in these two selection ogives. 
#' The max of the blue curve should also reach 0.7, I suspect that this error is due to the rounding inaccuracy.

#' resulting L50
res2$L50

#' resulting L75
res2$L75 

#' resulting wqs
(res2$L75 - res2$L50) * 2


#' This needs some proper testing, but on first glance, no pattern of the decreasing selectivity pattern for large individuals in the catch curve. Ascending part of the selectivity curve is relatively good represented. It might make more sense to run the catch curve with a smaller bin size to detect pattern on a finer scale in particular for larger length classes (possible to give general recommnedation for bin size in catch curve? - the smaller the better, always?). Aternative ways to test the behaviour and precision of the method:
#' 
#' - increase cv of Linf and K
#' - widen spawning window
#' - change relation of different gears (harvest_rate or with effort)
