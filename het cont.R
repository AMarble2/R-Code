
#hetogeneous Example

#Simulating Data 

n=100


set.seed(12345)
#Subgroup 1 control 
het.1.cont <- rnorm(n, 24.9, 2)
#Subgroup 1 treatment
het.1.treat <-rnorm(n,26.1,2) 

#male means and sds
het.1.cont.mean <- signif(mean(het.1.cont),3)
het.1.treat.mean <- signif(mean(het.1.treat),3)
het.1.cont.sd <- signif(sd(het.1.cont),3)
het.1.treat.sd <- signif(sd(het.1.treat),3)

het.1.cont.sd

set.seed(2468)
#Subgroup 2 control 
het.2.cont <- rnorm(n, 26.5, 3.5)
#Subgroup 2 treatment
het.2.treat <-rnorm(n,24.9,3.5) 

#male means and sds
het.2.cont.mean <- signif(mean(het.2.cont),3)
het.2.treat.mean <- signif(mean(het.2.treat),3)
het.2.cont.sd <- signif(sd(het.2.cont),3)
het.2.treat.sd <- signif(sd(het.2.treat),3)

#Collapsing subgroups

het.c.group.n <- 2*n #Assuming equal group sizes
het.c.N <- n*4 #Total study size 
het.c.cont.mean <- signif(((n*het.1.cont.mean + n*het.2.cont.mean) / het.c.group.n),3)
het.c.treat.mean <- signif(((n*het.1.treat.mean + n*het.2.treat.mean) / het.c.group.n),3)
het.c.cont.sd <- signif((sqrt(((n-1)*(het.1.cont.sd^2) + (n-1)*(het.2.cont.sd^2) + 
                                 ((n^2)/het.c.group.n)*(het.1.cont.mean^2 + het.2.cont.mean^2 
                                                        - 2*het.1.cont.mean*het.2.cont.mean))/(het.c.group.n-1))),3)
het.c.treat.sd <- signif((sqrt(((n-1)*(het.1.treat.sd^2) + (n-1)*(het.2.treat.sd^2) + 
                                  ((n^2)/het.c.group.n)*(het.1.treat.mean^2 + het.2.treat.mean^2 
                                                         - 2*het.1.treat.mean*het.2.treat.mean))/(het.c.group.n-1))),3)
het.c.cont.sd

#Calculating simple mean differences

het.1.MD <- signif((het.1.treat.mean - het.1.cont.mean),3)
het.2.MD <- signif((het.2.treat.mean - het.2.cont.mean),3)
het.c.MD <- signif((het.c.treat.mean - het.c.cont.mean),3)

het.1.MD
het.2.MD
het.c.MD



#MD using Metafor
library(metafor)
het.fxn.MD.1<- escalc("MD", m1i = het.1.treat.mean, m2i = het.1.cont.mean, 
                      sd1i = het.1.treat.sd, sd2i = het.1.cont.sd, n1i = n, n2i = n)
het.fxn.MD.2<- escalc("MD", m1i = het.2.treat.mean, m2i = het.2.cont.mean, 
                      sd1i = het.2.treat.sd, sd2i = het.2.cont.sd, n1i = n, n2i = n)
het.fxn.MD.c<- escalc("MD", m1i = het.c.treat.mean, m2i = het.c.cont.mean, 
                      sd1i = het.c.treat.sd, sd2i = het.c.cont.sd, 
                      n1i = het.c.group.n, n2i = het.c.group.n)
het.fxn.MD.1
het.fxn.MD.2
het.fxn.MD.c

het.fxn.MD.1.var <- het.fxn.MD.1$vi
het.fxn.MD.2.var <- het.fxn.MD.2$vi
het.fxn.MD.c.var <- het.fxn.MD.c$vi

#95% CI MD calculations 

het.1.MD.lb <- het.1.MD - 1.96*(sqrt(het.fxn.MD.1.var/n))
het.1.MD.ub <- het.1.MD + 1.96*(sqrt(het.fxn.MD.1.var/n))

het.2.MD.lb <- het.2.MD - 1.96*(sqrt(het.fxn.MD.2.var/n))
het.2.MD.ub <- het.2.MD + 1.96*(sqrt(het.fxn.MD.2.var/n))

het.c.MD.lb <- het.c.MD - 1.96*(sqrt(het.fxn.MD.c.var/n))
het.c.MD.ub <- het.c.MD + 1.96*(sqrt(het.fxn.MD.c.var/n))

het.1.MD.95.CI <- c(het.1.MD.lb, het.1.MD.ub)
het.2.MD.95.CI <- c(het.2.MD.lb, het.2.MD.ub)
het.c.MD.95.CI <- c(het.c.MD.lb, het.c.MD.ub)

het.1.MD.95.CI
het.2.MD.95.CI
het.c.MD.95.CI

#Variance calculations 

#Pooled variances 
het.1.var.pool <- signif(((((n-1)*((het.1.treat.sd)^2)) + ((n-1)*((het.1.cont.sd)^2))) / 
                            (n+n - 2)),3)
het.2.var.pool <- signif(((((n-1)*((het.2.treat.sd)^2)) + ((n-1)*((het.2.cont.sd)^2))) / 
                            (n+n - 2)),3)
het.c.var.pool <- signif(((((het.c.group.n-1)*((het.c.treat.sd)^2)) + 
                             ((het.c.group.n-1)*((het.c.cont.sd)^2))) / 
                            (het.c.group.n*2 - 2)),3)
#MD Variance 

het.1.MD.var <- signif(((1/n+1/n)*het.1.var.pool),3)
het.2.MD.var <- signif(((1/n+1/n)*het.2.var.pool),3)
het.c.MD.var <- signif(((1/het.c.group.n+1/het.c.group.n)*het.c.var.pool),3)

#Cohen's D Calculation

het.1.Cohen.D <- signif((het.1.MD / sqrt(het.1.var.pool)),2)
het.2.Cohen.D <- signif((het.2.MD / sqrt(het.2.var.pool)),2)
het.c.Cohen.D <- signif((het.c.MD / sqrt(het.c.var.pool)),2)

#95% CI Cohen's D
het.1.Cohen.D.lb <- het.1.Cohen.D - 1.96*(sqrt(het.1.Cohen.D.var/n))
het.1.Cohen.D.ub <- het.1.Cohen.D + 1.96*(sqrt(het.1.Cohen.D.var/n))

het.2.Cohen.D.lb <- het.2.Cohen.D - 1.96*(sqrt(het.2.Cohen.D.var/n))
het.2.Cohen.D.ub <- het.2.Cohen.D + 1.96*(sqrt(het.2.Cohen.D.var/n))

het.c.Cohen.D.lb <- het.c.Cohen.D - 1.96*(sqrt(het.c.Cohen.D.var/n))
het.c.Cohen.D.ub <- het.c.Cohen.D + 1.96*(sqrt(het.c.Cohen.D.var/n))

het.1.Cohen.D.95.CI <- c(het.1.Cohen.D.lb, het.1.Cohen.D.ub)
het.2.Cohen.D.95.CI <- c(het.2.Cohen.D.lb, het.2.Cohen.D.ub)
het.c.Cohen.D.95.CI <- c(het.c.Cohen.D.lb, het.c.Cohen.D.ub)

het.1.Cohen.D.95.CI
het.2.Cohen.D.95.CI
het.c.Cohen.D.95.CI

het.1.Cohen.D.var

#ESCALC only does hedges' g and not cohen's D.

#Approx. variance of Cohen's D 

het.1.Cohen.D.var <- 1/n + 1/n + (het.1.Cohen.D^2)/(2*(n+n))
het.2.Cohen.D.var <- 1/n + 1/n + het.2.Cohen.D^2/(2*(n+n))
het.c.Cohen.D.var <- 1/het.c.group.n + 1/het.c.group.n + het.2.Cohen.D^2/
  (2*(het.c.group.n+het.c.group.n))


#Hedges' g Calculation 

#Finding correction factor 

het.1.j <- het.2.j <- 1 - (3/ (4*(n+n-2) - 1))
het.c.j <- 1 - (3/ (4*(het.c.group.n+het.c.group.n-2) - 1))

#Calculating Hedges' g

het.1.hedge.g <- signif((het.1.j * het.1.Cohen.D),2)
het.2.hedge.g <- signif((het.2.j * het.2.Cohen.D),2)
het.c.hedge.g <- signif((het.c.j * het.c.Cohen.D),2)

het.1.hedge.g
het.2.hedge.g
het.c.hedge.g
#Hedges' g variance 

het.1.hedge.g.var <- het.1.j^2 * het.1.Cohen.D.var
het.2.hedge.g.var <- het.2.j^2 * het.2.Cohen.D.var
het.c.hedge.g.var <- het.c.j^2 * het.c.Cohen.D.var

het.1.hedge.g.var
het.2.hedge.g.var
het.c.hedge.g.var

#Hedge' g fxn 


het.fxn.hedges.g.1<- escalc("SMD", m1i = het.1.treat.mean, m2i = het.1.cont.mean, 
                            sd1i = het.1.treat.sd, sd2i = het.1.cont.sd, n1i = n, n2i = n)
het.fxn.hedges.g.2<- escalc("SMD", m1i = het.2.treat.mean, m2i = het.2.cont.mean, 
                            sd1i = het.2.treat.sd, sd2i = het.2.cont.sd, n1i = n, n2i = n)
het.fxn.hedges.g.c<- escalc("SMD", m1i = het.c.treat.mean, m2i = het.c.cont.mean, 
                            sd1i = het.c.treat.sd, sd2i = het.c.cont.sd, 
                            n1i = het.c.group.n, n2i = het.c.group.n)

het.fxn.hedges.g.1
het.fxn.hedges.g.2
het.fxn.hedges.g.c

het.fxn.hedges.g.1.var <- het.fxn.hedges.g.1$vi
het.fxn.hedges.g.2.var <- het.fxn.hedges.g.2$vi
het.fxn.hedges.g.c.var <- het.fxn.hedges.g.c$vi

#95% CI MD calculations 

het.1.hedges.g.lb <- het.1.hedge.g - 1.96*(sqrt(het.fxn.hedges.g.1.var/n))
het.1.hedges.g.ub <- het.1.hedge.g + 1.96*(sqrt(het.fxn.hedges.g.1.var/n))

het.2.hedges.g.lb <- het.2.hedge.g - 1.96*(sqrt(het.fxn.hedges.g.2.var/n))
het.2.hedges.g.ub <- het.2.hedge.g + 1.96*(sqrt(het.fxn.hedges.g.2.var/n))

het.c.hedges.g.lb <- het.c.hedge.g - 1.96*(sqrt(het.fxn.hedges.g.c.var/n))
het.c.hedges.g.ub <- het.c.hedge.g + 1.96*(sqrt(het.fxn.hedges.g.c.var/n))

het.1.hedges.g.95.CI <- c(het.1.hedges.g.lb, het.1.hedges.g.ub)
het.2.hedges.g.95.CI <- c(het.2.hedges.g.lb, het.2.hedges.g.ub)
het.c.hedges.g.95.CI <- c(het.c.hedges.g.lb, het.c.hedges.g.ub)

het.1.hedges.g.95.CI
het.2.hedges.g.95.CI
het.c.hedges.g.95.CI
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

#Using Common Effects Model to combine effect sizes 

#MD Calculation 

#Creating weights 

het.1.MD.weight <- 1/het.1.MD.var
het.2.MD.weight <- 1/het.2.MD.var

#Weighted mean (i.e., Common Effect MD )  
het.num.1.MD <- het.1.MD.weight * het.1.MD
het.num.2.MD <- het.2.MD.weight * het.2.MD
het.CE.MD <- signif(((het.num.1.MD + het.num.2.MD)/ 
                       (het.1.MD.weight + het.2.MD.weight)),2)

het.CE.MD
#Variance of CE MD estimate
het.CE.MD.var <- signif(((1 / (het.1.MD.weight + het.2.MD.weight))),2)
het.CE.MD.var

#Using Metafor functions 


#Cohen's D Calculation 

#Creating weights 

het.1.Cohen.D.weight <- 1/het.1.Cohen.D.var
het.2.Cohen.D.weight <- 1/het.1.Cohen.D.var

#Weighted mean (i.e., Common Effect MD )  
het.num.1.Cohen.D <- het.1.Cohen.D.weight * het.1.Cohen.D
het.num.2.Cohen.D <- het.2.Cohen.D.weight * het.2.Cohen.D
het.CE.Cohen.D <- signif(((het.num.1.Cohen.D + het.num.2.Cohen.D)/ 
                            (het.1.Cohen.D.weight + het.2.Cohen.D.weight)),2)

het.CE.Cohen.D
#Variance of CE MD estimate
het.CE.Cohen.D.var <- signif(((1 / (het.1.Cohen.D.weight + het.2.Cohen.D.weight))),2)

#Cohen's D Calculation 

#Creating weights 

het.1.hedge.g.weight <- 1/het.1.hedge.g.var
het.2.hedge.g.weight <- 1/het.2.hedge.g.var

#Weighted mean (i.e., Common Effect MD )  
het.num.1.hedge.g <- het.1.hedge.g.weight * het.1.hedge.g
het.num.2.hedge.g <- het.2.hedge.g.weight * het.2.hedge.g
het.CE.hedge.g <- signif(((het.num.1.hedge.g + het.num.2.hedge.g)/ 
                            (het.1.hedge.g.weight + het.2.hedge.g.weight)),2)
het.CE.hedge.g

#Variance of CE MD estimate
het.CE.hedge.g.var <- signif(((1 / (het.1.hedge.g.weight + het.2.hedge.g.weight ))),2)




#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

#Random effects model 

het.re.MD.comb <- rma.uni(yi = c(het.1.MD, het.2.MD), 
                          vi = c(het.1.MD.var, het.2.MD.var))
                          
het.re.MD <- het.re.MD.comb$b
het.re.MD.var <- het.re.MD.comb$vb




#Cohen's D
het.re.CD.comb <-rma.uni(yi = c(het.1.Cohen.D, het.2.Cohen.D), 
                         vi = c(het.1.Cohen.D.var, het.2.Cohen.D.var))

het.re.CD <- het.re.CD.comb$b
het.re.CD.var <- het.re.CD.comb$vb

#Hedges' g 
het.re.hg.comb<-rma.uni(yi = c(het.1.hedge.g, het.2.hedge.g), 
                        vi = c(het.1.hedge.g.var, het.2.hedge.g.var))

het.re.hg <- het.re.hg.comb$b
het.re.hg.var <- het.re.hg.comb$vb


#Common Effects 

#MD
het.ce.MD.comb <- rma.uni(yi = c(het.1.MD, het.2.MD), 
        vi = c(het.1.MD.var, het.2.MD.var),
        method="FE")
het.ce.MD <- het.ce.MD.comb$b
het.ce.MD.var <- het.ce.MD.comb$vb

#Cohen's D
het.ce.CD.comb <-rma.uni(yi = c(het.1.Cohen.D, het.2.Cohen.D), 
        vi = c(het.1.Cohen.D.var, het.2.Cohen.D.var),
        method="FE")

het.ce.CD <- het.ce.CD.comb$b
het.ce.CD.var <- het.ce.CD.comb$vb


#Hedges' g 
het.ce.hg.comb<-rma.uni(yi = c(het.1.hedge.g, het.2.hedge.g), 
        vi = c(het.1.hedge.g.var, het.2.hedge.g.var),
        method = "FE")

het.ce.hg <- het.ce.hg.comb$b
het.ce.hg.var <- het.ce.hg.comb$vb
