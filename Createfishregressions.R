#Travel cost data to plot


#how-to in excel: https://www.excel-easy.com/examples/regression.html

#Class exercise: plot Peter Berck's data: 
#Zone=c(1,2,3,4,5)
#Price=c(0.9,1.95,3,3.95,5)
#Trips=c(28,17,14,12,5)
#travelreg<-lm(Trips~Price)


#plot(Trips, Price, pch=16, cex=1.3, col = “blue”, main=”Demand for Beach”, xlab= “trips”, ylab=””price”)
#plot(Price, Trips)
#summary(travelreg)
#abline(travelreg)

#should also try poisson model or negative binomial 
#https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/

require(MASS)
fish.nb1 <- glm.nb(vizmarina ~ cost, data = fish)
fish.nb1
summary(fish.nb1)


####################################cool code on rep()
rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,2))     # same as second.
rep(1:4, c(2,1,2,1))
rep(1:4, each = 2, length.out = 4)    # first 4 only.
rep(1:4, each = 2, length.out = 10)   # 8 integers plus two recycled 1's.
rep(1:4, each = 2, times = 3)         # length 24, 3 complete replications

rep(1, 40*(1-.8)) # length 7 on most platforms
rep(1, 40*(1-.8)+1e-7) # better

## replicate a list
fred <- list(happy = 1:10, name = "squash")
rep(fred, 5)

# date-time objects
x <- .leap.seconds[1:3]
rep(x, 2)
rep(as.POSIXlt(x), rep(2, 3))

## named factor
x <- factor(LETTERS[1:4]); names(x) <- letters[1:4]
x
rep(x, 2)
rep(x, each = 2)
rep.int(x, 2)  # no names
rep_len(x, 10)

################################################

###############################tcm regs with diff configs of data

> zonenames <- factor(LETTERS[1:4]); names(x) <- letters[1:4]
> zonenames
> zone<-rep(zonenames, c(100, 75, 50, 25))

> costlevel<-c(1, 3, 3.5, 5)
> cost<-rep(costlevel, c(100, 75, 50, 25))
> viz<-c(100, 75, 50, 25)
> viz
> visits<-rep(viz, c(100, 75, 50, 25))
> set<-data.frame(zone, visits, cost)
> View(set)

> tcmreg1<-lm(visits~cost)
> plot(cost, visits)
> abline(tcmreg1)

#Using a Vpc approach
> Price=c(1, 3, 3.5, 5)
> Trips=c(100, 75, 50, 25)

tcmreg2<-lm(Trips~Price)
summary(tcmreg2)
plot(Price, Trips)
abline(tcmreg2)

#using Berck's original example in Revealed Pref Methods slides from 2010
#price per per trip for each person
Ppertrip=c(.9, 1.95, 3, 3.95, 5)
Qperindivid=c(28, 17, 14, 12, 5) 
tcmr3<-lm(Qperindivid~Ppertrip)

######## get a line approx:  Q = 30-5*P
summary(tcmr3)
plot(Ppertrip, Qperindivid)
abline(tcmr3)





#trying jellybear reg
vpc<-c(.85,.55,.25,0)
dollars<-c(1,3,5,7)
tcmreg3<-lm(vpc~dollars)
summary(tcmreg3)


