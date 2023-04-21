
#Homogeneous Example

#Simulating Data 

n=100


set.seed(12346)
#Subgroup 1 control 
hom.1.cont <- rnorm(n, 26.6, 3.5)
#Subgroup 1 treatment
hom.1.treat <-rnorm(n,24.9,3.5) 

#male means and sds
hom.1.cont.mean <- signif(mean(hom.1.cont),3)
hom.1.treat.mean <- signif(mean(hom.1.treat),3)
hom.1.cont.sd <- signif(sd(hom.1.cont),3)
hom.1.treat.sd <- signif(sd(hom.1.treat),3)
hom.1.cont.mean

#Subgroup 2 control 
#hom.2.cont <- rnorm(n, 30.0, 3.5)
#Subgroup 2 treatment
#hom.2.treat <-rnorm(n,28.3,3.5) 
#male means and sds
hom.2.cont.mean <- 30.0
hom.2.treat.mean <- 28.7
hom.2.cont.sd <- 3.90
hom.2.treat.sd <- 3.98
#-1.3
hom.2.treat.mean - hom.2.cont.mean
hom.2.cont.sd
hom.2.treat.sd
#Collapsing subgroups

hom.c.group.n <- 2*n #Assuming equal group sizes
hom.c.N <- n*4 #Total study size 
hom.c.cont.mean <- ((n*hom.1.cont.mean + n*hom.2.cont.mean) / hom.c.group.n)
hom.c.treat.mean <- ((n*hom.1.treat.mean + n*hom.2.treat.mean) / hom.c.group.n)
hom.c.cont.sd <- sqrt(((n-1)*(hom.1.cont.sd^2) + (n-1)*(hom.2.cont.sd^2) + 
                     ((n^2)/hom.c.group.n)*(hom.1.cont.mean^2 + hom.2.cont.mean^2 
                        - 2*hom.1.cont.mean*hom.2.cont.mean))/(hom.c.group.n-1))
hom.c.treat.sd <- sqrt(((n-1)*(hom.1.treat.sd^2) + (n-1)*(hom.2.treat.sd^2) + 
                      ((n^2)/hom.c.group.n)*(hom.1.treat.mean^2 + hom.2.treat.mean^2 
                        - 2*hom.1.treat.mean*hom.2.treat.mean))/(hom.c.group.n-1))
num <- (n-1)*(hom.1.cont.sd^2) + (n-1)*(hom.2.cont.sd^2) + ((n^2)/(2*n))*
        (hom.1.cont.mean^2 + hom.2.cont.mean^2 - 2*hom.1.cont.mean*hom.2.cont.mean)


hom.c.cont.sd

#Calculating simple mean differences

hom.1.MD <- hom.1.treat.mean - hom.1.cont.mean
hom.2.MD <- hom.2.treat.mean - hom.2.cont.mean
hom.c.MD <- hom.c.treat.mean - hom.c.cont.mean


hom.1.MD
hom.2.MD
hom.c.MD



#MD using Metafor
library(metafor)
hom.fxn.MD.1<- escalc("MD", m1i = hom.1.treat.mean, m2i = hom.1.cont.mean, 
                      sd1i = hom.1.treat.sd, sd2i = hom.1.cont.sd, n1i = n, n2i = n)
hom.fxn.MD.2<- escalc("MD", m1i = hom.2.treat.mean, m2i = hom.2.cont.mean, 
                      sd1i = hom.2.treat.sd, sd2i = hom.2.cont.sd, n1i = n, n2i = n)
hom.fxn.MD.c<- escalc("MD", m1i = hom.c.treat.mean, m2i = hom.c.cont.mean, 
                      sd1i = hom.c.treat.sd, sd2i = hom.c.cont.sd, 
                      n1i = hom.c.group.n, n2i = hom.c.group.n)

hom.fxn.MD.1
hom.fxn.MD.2
hom.fxn.MD.c

hom.fxn.MD.1.var <- hom.fxn.MD.1$vi
hom.fxn.MD.2.var <- hom.fxn.MD.2$vi
hom.fxn.MD.c.var <- hom.fxn.MD.c$vi

#95% CI MD calculations 

hom.1.MD.lb <- hom.1.MD - 1.96*(sqrt(hom.fxn.MD.1.var/n))
hom.1.MD.ub <- hom.1.MD + 1.96*(sqrt(hom.fxn.MD.1.var/n))

hom.2.MD.lb <- hom.2.MD - 1.96*(sqrt(hom.fxn.MD.2.var/n))
hom.2.MD.ub <- hom.2.MD + 1.96*(sqrt(hom.fxn.MD.2.var/n))

hom.c.MD.lb <- hom.c.MD - 1.96*(sqrt(hom.fxn.MD.c.var/n))
hom.c.MD.ub <- hom.c.MD + 1.96*(sqrt(hom.fxn.MD.c.var/n))

hom.1.MD.95.CI <- c(hom.1.MD.lb, hom.1.MD.ub)
hom.2.MD.95.CI <- c(hom.2.MD.lb, hom.2.MD.ub)
hom.c.MD.95.CI <- c(hom.c.MD.lb, hom.c.MD.ub)

hom.1.MD.95.CI
hom.2.MD.95.CI
hom.c.MD.95.CI



#Variance calculations 

#Pooled variances 
hom.1.var.pool <- ((((n-1)*((hom.1.treat.sd)^2)) + ((n-1)*((hom.1.cont.sd)^2))) / 
                           (n+n - 2)) #For controls
hom.2.var.pool <- ((((n-1)*((hom.2.treat.sd)^2)) + ((n-1)*((hom.2.cont.sd)^2))) / 
                           (n+n - 2)) #For treatments
hom.c.var.pool <- ((((hom.c.group.n-1)*((hom.c.treat.sd)^2)) + 
                            ((hom.c.group.n-1)*((hom.c.cont.sd)^2))) / 
                            (hom.c.group.n*2 - 2))

hom.1.var.pool
hom.2.var.pool 
hom.c.var.pool

#MD Variance 

hom.1.MD.var <- ((1/n+1/n)*hom.1.var.pool)
hom.2.MD.var <- ((1/n+1/n)*hom.2.var.pool)
hom.c.MD.var <- ((1/hom.c.group.n+1/hom.c.group.n)*hom.c.var.pool)


#Cohen's D Calculation

hom.1.Cohen.D <- (hom.1.MD / sqrt(hom.1.var.pool))
hom.2.Cohen.D <- (hom.2.MD / sqrt(hom.2.var.pool))
hom.c.Cohen.D <- (hom.c.MD / sqrt(hom.c.var.pool))

hom.1.Cohen.D
hom.2.Cohen.D
hom.c.Cohen.D

#ESCALC only does hedges' g and not cohen's D.

#Approx. variance of Cohen's D 

hom.1.Cohen.D.var <- 1/n + 1/n + hom.1.Cohen.D^2/(2*(n+n))
hom.2.Cohen.D.var <- 1/n + 1/n + hom.2.Cohen.D^2/(2*(n+n))
hom.c.Cohen.D.var <- 1/hom.c.group.n + 1/hom.c.group.n + hom.2.Cohen.D^2/
                    (2*(hom.c.group.n+hom.c.group.n))



#95% CI Cohen's D
hom.1.Cohen.D.lb <- hom.1.Cohen.D - 1.96*(sqrt(hom.1.Cohen.D.var/n))
hom.1.Cohen.D.ub <- hom.1.Cohen.D + 1.96*(sqrt(hom.1.Cohen.D.var/n))

hom.2.Cohen.D.lb <- hom.2.Cohen.D - 1.96*(sqrt(hom.2.Cohen.D.var/n))
hom.2.Cohen.D.ub <- hom.2.Cohen.D + 1.96*(sqrt(hom.2.Cohen.D.var/n))

hom.c.Cohen.D.lb <- hom.c.Cohen.D - 1.96*(sqrt(hom.c.Cohen.D.var/n))
hom.c.Cohen.D.ub <- hom.c.Cohen.D + 1.96*(sqrt(hom.c.Cohen.D.var/n))

hom.1.Cohen.D.95.CI <- c(hom.1.Cohen.D.lb, hom.1.Cohen.D.ub)
hom.2.Cohen.D.95.CI <- c(hom.2.Cohen.D.lb, hom.2.Cohen.D.ub)
hom.c.Cohen.D.95.CI <- c(hom.c.Cohen.D.lb, hom.c.Cohen.D.ub)

hom.1.Cohen.D.95.CI
hom.2.Cohen.D.95.CI
hom.c.Cohen.D.95.CI





#Hedges' g Calculation 

#Finding correction factor 

hom.1.j <- hom.2.j <- 1 - (3/ (4*(n+n-2) - 1))
hom.c.j <- 1 - (3/ (4*(hom.c.group.n+hom.c.group.n-2) - 1))

#Calculating Hedges' g

hom.1.hedge.g <- signif((hom.1.j * hom.1.Cohen.D),2)
hom.2.hedge.g <- signif((hom.2.j * hom.2.Cohen.D),2)
hom.c.hedge.g <- signif((hom.c.j * hom.c.Cohen.D),2)

#Hedges' g variance 

hom.1.hedge.g.var <- hom.1.j^2 * hom.1.Cohen.D.var
hom.2.hedge.g.var <- hom.2.j^2 * hom.2.Cohen.D.var
hom.c.hedge.g.var <- hom.c.j^2 * hom.c.Cohen.D.var

hom.1.hedge.g.var
hom.2.hedge.g.var
hom.c.hedge.g.var


#Hedge' g fxn 

hom.fxn.hedges.g.1<- escalc("SMD", m1i = hom.1.treat.mean, m2i = hom.1.cont.mean, 
                           sd1i = hom.1.treat.sd, sd2i = hom.1.cont.sd, n1i = n, n2i = n)
hom.fxn.hedges.g.2<- escalc("SMD", m1i = hom.2.treat.mean, m2i = hom.2.cont.mean, 
                           sd1i = hom.2.treat.sd, sd2i = hom.2.cont.sd, n1i = n, n2i = n)
hom.fxn.hedges.g.c<- escalc("SMD", m1i = hom.c.treat.mean, m2i = hom.c.cont.mean, 
                           sd1i = hom.c.treat.sd, sd2i = hom.c.cont.sd, 
                           n1i = hom.c.group.n, n2i = hom.c.group.n)

hom.fxn.hedges.g.1
hom.fxn.hedges.g.2
hom.fxn.hedges.g.c

hom.fxn.hedges.g.1.var <- hom.fxn.hedges.g.1$vi
hom.fxn.hedges.g.2.var <- hom.fxn.hedges.g.2$vi
hom.fxn.hedges.g.c.var <- hom.fxn.hedges.g.c$vi

#95% CI MD calculations 

hom.1.hedges.g.lb <- hom.1.hedge.g - 1.96*(sqrt(hom.fxn.hedges.g.1.var/n))
hom.1.hedges.g.ub <- hom.1.hedge.g + 1.96*(sqrt(hom.fxn.hedges.g.1.var/n))

hom.2.hedges.g.lb <- hom.2.hedge.g - 1.96*(sqrt(hom.fxn.hedges.g.2.var/n))
hom.2.hedges.g.ub <- hom.2.hedge.g + 1.96*(sqrt(hom.fxn.hedges.g.2.var/n))

hom.c.hedges.g.lb <- hom.c.hedge.g - 1.96*(sqrt(hom.fxn.hedges.g.c.var/n))
hom.c.hedges.g.ub <- hom.c.hedge.g + 1.96*(sqrt(hom.fxn.hedges.g.c.var/n))

hom.1.hedges.g.95.CI <- c(hom.1.hedges.g.lb, hom.1.hedges.g.ub)
hom.2.hedges.g.95.CI <- c(hom.2.hedges.g.lb, hom.2.hedges.g.ub)
hom.c.hedges.g.95.CI <- c(hom.c.hedges.g.lb, hom.c.hedges.g.ub)

hom.1.hedges.g.95.CI
hom.2.hedges.g.95.CI
hom.c.hedges.g.95.CI

#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

#Using Common Effects Model to combine effect sizes 

#MD Calculation 

#Creating weights 

hom.1.MD.weight <- 1/hom.1.MD.var
hom.2.MD.weight <- 1/hom.2.MD.var

#Weighted mean (i.e., Common Effect MD )  
hom.num.1.MD <- hom.1.MD.weight * hom.1.MD
hom.num.2.MD <- hom.2.MD.weight * hom.2.MD
hom.CE.MD <- signif(((hom.num.1.MD + hom.num.2.MD)/ 
                       (hom.1.MD.weight + hom.2.MD.weight)),2)
hom.CE.MD
#Variance of CE MD estimate
hom.CE.MD.var <- signif(((1 / (hom.1.MD.weight + hom.2.MD.weight))),2)
hom.CE.MD.var

#Using Metafor functions 


#Cohen's D Calculation 

#Creating weights 

hom.1.Cohen.D.weight <- 1/hom.1.Cohen.D.var
hom.2.Cohen.D.weight <- 1/hom.1.Cohen.D.var

#Weighted mean (i.e., Common Effect MD )  
hom.num.1.Cohen.D <- hom.1.Cohen.D.weight * hom.1.Cohen.D
hom.num.2.Cohen.D <- hom.2.Cohen.D.weight * hom.2.Cohen.D
hom.CE.Cohen.D <- signif(((hom.num.1.Cohen.D + hom.num.2.Cohen.D)/ 
                       (hom.1.Cohen.D.weight + hom.2.Cohen.D.weight)),2)

#Variance of CE Cohen estimate
hom.CE.Cohen.D.var <- signif(((1 / (hom.1.Cohen.D.weight + hom.2.Cohen.D.weight))),2)

hom.CE.Cohen.D.var
#Cohen's D Calculation 

#Creating weights 

hom.1.hedge.g.weight <- 1/hom.1.hedge.g.var
hom.2.hedge.g.weight <- 1/hom.2.hedge.g.var

#Weighted mean (i.e., Common Effect MD )  
hom.num.1.hedge.g <- hom.1.hedge.g.weight * hom.1.hedge.g
hom.num.2.hedge.g <- hom.2.hedge.g.weight * hom.2.hedge.g
hom.CE.hedge.g <- signif(((hom.num.1.hedge.g + hom.num.2.hedge.g)/ 
                            (hom.1.hedge.g.weight + hom.2.hedge.g.weight)),2)
hom.CE.hedge.g

#Variance of CE MD estimate
hom.CE.hedge.g.var <- signif(((1 / (hom.1.hedge.g.weight + hom.2.hedge.g.weight ))),2)

hom.CE.hedge.g.var

#Using Functions 

#Random Effects 
#MD
hom.re.MD.comb <- rma(yi = c(hom.1.MD, hom.2.MD),
    vi = c(hom.1.MD.var, hom.2.MD.var))

hom.re.MD.comb
hom.re.MD <- hom.ce.MD.comb$beta
hom.re.MD.var <- hom.ce.MD.comb$vb


#Cohen's D
hom.re.CD.comb <- rma(yi = c(hom.1.Cohen.D, hom.2.Cohen.D),
    vi = c(hom.1.Cohen.D.var, hom.2.Cohen.D.var))

hom.re.CD <- hom.ce.CD.comb$beta
hom.re.CD.var <- hom.ce.CD.comb$vb

hom.re.CD.comb

#Hedge's g
hom.re.HG.comb <- rma(yi = c(hom.1.hedge.g, hom.2.hedge.g),
    vi = c(hom.1.hedge.g.var, hom.2.hedge.g.var))

hom.re.hg <- hom.ce.HG.comb$b
hom.re.hg.var <- hom.ce.HG.comb$vb

#Common Effects
#MD
rma(yi = c(hom.1.MD, hom.2.MD),
    vi = c(hom.1.MD.var, hom.2.MD.var),
    method="FE")

#Cohen's D
rma(yi = c(hom.1.Cohen.D, hom.2.Cohen.D),
    vi = c(hom.1.Cohen.D.var, hom.2.Cohen.D.var),
    method="FE")

#Hedge's g
rma(yi = c(hom.1.hedge.g, hom.2.hedge.g),
    vi = c(hom.1.hedge.g.var, hom.2.hedge.g.var),
    method="FE")
