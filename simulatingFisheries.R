#'---
#' title: Simulating fisheries with specific selectivity characteristics
#' author: Tobias Mildenberger
#' date: 30/06/2017
#' export: html_document
#'---
#'
require(TropFishR.select)
#'
#' ## Introduction to stockSim.R
Linf.mu = 80
L50 = 0.25*Linf.mu
wqs = L50*0.2
ex1 <- stockSim(Linf.mu = Linf.mu, L50 = L50,
                wqs = wqs)

#' Fishing mortality
plot(as.Date(ex1$fisheries$fished_years, "%Y-%m-%d"), ex1$fisheries$F, 
     type = 'l', main = "Constant fishing mortality",
     xlab = "Year", ylab = "F", lwd=2)

#' Selectivity characteristics
Lt = seq(0,Linf.mu,0.01)
pt1 <-  logisticSelect(Lt,L50,wqs) * 100
plot(Lt,pt1,type="l", col = 'blue',lwd=2,
     main = "Selectivity curve",
     xlab = "Length", ylab = "Probability of capture")

#' Resulting LFQ data
require(TropFishR)
class(ex1$lfqbin) <- "lfq"
plot(ex1$lfqbin, Fname = "catch")

