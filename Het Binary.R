library(metafor)
#het OR 
#optimizing OR Cells for OR = 0.70
x <- seq(1,100,by=1)
e <- matrix(nrow=100, ncol=4)
for (i in 1:100) {
  a <- x[i]
  b <- 100-a
  c <- (400*a)/(a+300)
  d <- -(300*(a-100))/(a+300)
  e[i,1:4] <- c(a,b,c,d)
}

e

#optimizing RR Cells for RR = 0.70
x <- seq(1,100,by=1)
e <- matrix(nrow=100, ncol=4)
for (i in 1:100) {
  a <- x[i]
  b <- 100-a
  c <- (10*a) / 7
  d <- -(10/7)*(a-70)
  e[i,1:4] <- c(a,b,c,d)
}

e
#Subgroup 1
het.OR.1 <- matrix(nrow=3, ncol=3)

het.vn.OR.1 <- c(20,80,25,75) # Vector of cell values c(a,b,c,d)

het.OR.a.1 <-het.OR.1[1,1]<- het.vn.OR.1[1] #Cell a
het.OR.b.1 <- het.OR.1[1,2] <- het.vn.OR.1[2] #Cell b
het.OR.c.1 <- het.OR.1[2,1] <- het.vn.OR.1[3] #Cell c
het.OR.d.1 <- het.OR.1[2,2]<- het.vn.OR.1[4] #Cell d
het.OR.treat.n1<- het.OR.1[1,3] <- het.OR.a.1 + het.OR.b.1 #n Treatment
het.OR.cont.n1 <- het.OR.1[2,3] <- het.OR.c.1 + het.OR.d.1 #n Control
het.OR.e.n1 <- het.OR.1[3,1] <- het.OR.a.1 + het.OR.c.1 #n Event
het.OR.ne.n1 <- het.OR.1[3,2] <- het.OR.b.1 + het.OR.d.1 #n No Event
het.OR.N1 <- het.OR.1[3,3] <- het.OR.a.1 + het.OR.b.1 + het.OR.c.1 + het.OR.d.1 #Total Sub1 

#Subgroup 2
het.OR.2 <- matrix(nrow=3, ncol=3)

het.vn.OR.2 <- c(40,60,25,75) # Vector of cell values c(a,b,c,d)

het.OR.a.2 <-het.OR.2[1,1]<- het.vn.OR.2[1] #Cell a
het.OR.b.2 <- het.OR.2[1,2] <- het.vn.OR.2[2] #Cell b
het.OR.c.2 <- het.OR.2[2,1] <- het.vn.OR.2[3] #Cell c
het.OR.d.2 <- het.OR.2[2,2]<- het.vn.OR.2[4] #Cell d
het.OR.treat.n2<- het.OR.2[1,3] <- het.OR.a.2 + het.OR.b.2 #n Treatment
het.OR.cont.n2 <- het.OR.2[2,3] <- het.OR.c.2 + het.OR.d.2 #n Control
het.OR.e.n2 <- het.OR.2[3,1] <- het.OR.a.2 + het.OR.c.2 #n Event
het.OR.ne.n2 <- het.OR.2[3,2] <- het.OR.b.2 + het.OR.d.2 #n No Event
het.OR.N2 <- het.OR.2[3,3] <- het.OR.a.2 + het.OR.b.2 + het.OR.c.2 + het.OR.d.2 #Total Sub2

#Collapsed 

het.OR.c <- matrix(nrow=3, ncol=3)

het.vn.OR.c <- c(het.OR.a.1+het.OR.a.2,het.OR.b.1+het.OR.b.2,
                 het.OR.c.1+het.OR.c.2,het.OR.d.1+het.OR.d.2) # Vector of cell values c(a,b,c,d)

het.OR.a.c <-het.OR.c[1,1]<- het.vn.OR.c[1] #Cell a
het.OR.b.c <- het.OR.c[1,2] <- het.vn.OR.c[2] #Cell b
het.OR.c.c <- het.OR.c[2,1] <- het.vn.OR.c[3] #Cell c
het.OR.d.c <- het.OR.c[2,2]<- het.vn.OR.c[4] #Cell d
het.OR.treat.nc<- het.OR.c[1,3] <- het.OR.a.c + het.OR.b.c #n Treatment
het.OR.cont.nc <- het.OR.c[2,3] <- het.OR.c.c + het.OR.d.c #n Control
het.OR.e.nc <- het.OR.c[3,1] <- het.OR.a.c + het.OR.c.c #n Event
het.OR.ne.nc <- het.OR.c[3,2] <- het.OR.b.c + het.OR.d.c #n No Event
het.OR.Nc <- het.OR.c[3,3] <- het.OR.a.c + het.OR.b.c + het.OR.c.c + het.OR.d.c #Total Collapsed

#OR Calculations

OR.het.1 <- (het.OR.a.1*het.OR.d.1) / (het.OR.b.1*het.OR.c.1)
OR.het.2 <- (het.OR.a.2*het.OR.d.2) / (het.OR.b.2*het.OR.c.2)
OR.het.c <- (het.OR.a.c*het.OR.d.c) / (het.OR.b.c*het.OR.c.c) 

OR.het.c



#OR Variance calculations

OR.het.1.var.calc <- 1/het.OR.a.1 + 1/het.OR.b.1 + 1/het.OR.c.1 + 1/het.OR.d.1
OR.het.2.var.calc <- 1/het.OR.a.2 + 1/het.OR.b.2 + 1/het.OR.c.2 + 1/het.OR.d.2
OR.het.c.var.calc <- 1/het.OR.a.c + 1/het.OR.b.c + 1/het.OR.c.c + 1/het.OR.d.c

#OR 95% CI calculations 
OR.het.1.95.n <- log(OR.het.1) - 1.96*sqrt(OR.het.1.var.calc)
OR.het.1.95.p <- log(OR.het.1) + 1.96*sqrt(OR.het.1.var.calc)

OR.het.2.95.n <- log(OR.het.2) - 1.96*sqrt(OR.het.2.var.calc)
OR.het.2.95.p <- log(OR.het.2) + 1.96*sqrt(OR.het.2.var.calc)

OR.het.c.95.n <- log(OR.het.c) - 1.96*sqrt(OR.het.c.var.calc)
OR.het.c.95.p <- log(OR.het.c) + 1.96*sqrt(OR.het.c.var.calc)

OR.het.1.CI<-c(exp(OR.het.1.95.n), exp(OR.het.1.95.p))
OR.het.2.CI<-c(exp(OR.het.2.95.n), exp(OR.het.2.95.p))
OR.het.c.CI<-c(exp(OR.het.c.95.n), exp(OR.het.c.95.p))

OR.het.1.CI
OR.het.2.CI
OR.het.c.CI




#Using ESCALC OR 

OR.het.1.var.int <-escalc("OR", het.OR.a.1, het.OR.b.1, het.OR.c.1, het.OR.d.1)
OR.het.2.var.int <-escalc("OR", het.OR.a.2, het.OR.b.2, het.OR.c.2, het.OR.d.2)
OR.het.c.var.int <- escalc("OR", het.OR.a.c, het.OR.b.c, het.OR.c.c, het.OR.d.c)

OR.het.1.var <- OR.het.1.var.int$vi
OR.het.2.var <- OR.het.2.var.int$vi
OR.het.c.var <- OR.het.c.var.int$vi

OR.het.1.var
OR.het.2.var
OR.het.c.var

escalc("OR", het.OR.a.1, het.OR.b.1, het.OR.c.1, het.OR.d.1)
#OR MH Calculations 

het.numerator.1.MH.OR <- (het.OR.a.1*het.OR.d.1) / het.OR.N1
het.denom.1.MH.OR <- (het.OR.b.1*het.OR.c.1) / het.OR.N1
het.numerator.2.MH.OR <- (het.OR.a.2*het.OR.d.2) / het.OR.N2
het.denom.2.MH.OR <- (het.OR.b.2*het.OR.c.2) / het.OR.N2
OR.het.MH <- sum(het.numerator.1.MH.OR,het.numerator.2.MH.OR) / 
  sum(het.denom.1.MH.OR,het.denom.2.MH.OR)

OR.het.MH

#Variance estimator for OR MH 

het.P1 <- (het.OR.a.1+het.OR.d.1)/n
het.Q1 <- (het.OR.b.1+het.OR.c.1)/n
het.R1 <- (het.OR.a.1*het.OR.d.1)/n
het.S1 <- (het.OR.b.1*het.OR.c.1)/n
het.R1P1 <- het.R1*het.P1
het.P1S1 <- het.P1*het.S1
het.Q1R1 <- het.Q1*het.R1
het.S1Q1 <- het.S1*het.Q1
het.P2 <- (het.OR.a.2+het.OR.d.2)/n
het.Q2 <- (het.OR.b.2+het.OR.c.2)/n
het.R2 <- (het.OR.a.2*het.OR.d.2)/n
het.S2 <- (het.OR.b.2*het.OR.c.2)/n
het.R2P2 <- het.R2*het.P2
het.P2S2 <- het.P2*het.S2
het.Q2R2 <- het.Q2*het.R2
het.S2Q2 <- het.S2*het.Q2

het.num.MH.1 <- sum(het.R1P1, het.R2P2)
het.num.MH.2 <- sum(het.P1S1+het.Q1R1, het.P2S2+het.Q2R2)
het.num.MH.3 <- sum(het.S1Q1, het.S2Q2)
het.den.MH.1 <- 2*((sum(het.R1, het.R2))^2)
het.den.MH.2 <- 2*((sum(het.R1,het.R2))*(sum(het.S1,het.S2)))
het.den.MH.3 <- 2*((sum(het.R1,het.R2))*(sum(het.S1,het.S2)^2))

OR.het.MH.var <- het.num.MH.1/het.den.MH.1 +
  het.num.MH.2/het.den.MH.2+
  het.num.MH.2/het.den.MH.2
OR.het.MH.var


#Using R Function MH OR
library(metafor)

het.fxn.MH.OR.c<-rma.mh(measure="OR", c(het.OR.a.1, het.OR.a.2),
                        c(het.OR.b.1, het.OR.b.2),
                        c(het.OR.c.1, het.OR.c.2),
                        c(het.OR.d.1, het.OR.d.2),
                        c(het.OR.treat.n1, het.OR.treat.n2),
                        c(het.OR.cont.n1, het.OR.cont.n2),
                        c(het.OR.e.n1, het.OR.e.n2),
                        c(het.OR.ne.n1, het.OR.ne.n2))



#Peto OR Calculation

#Subgroup 1
het.1.o <- het.OR.a.1
het.1.e <- ((het.OR.a.1 + het.OR.b.1)*(het.OR.a.1 + het.OR.c.1)) / het.OR.N1
het.1.v.num <- (het.OR.a.1 + het.OR.b.1)*(het.OR.c.1 + het.OR.d.1)*
  (het.OR.a.1 + het.OR.c.1)*(het.OR.b.1 + het.OR.d.1)
het.1.v.denom <- (het.OR.N1^2)*(het.OR.N1-1)
het.1.v <- het.1.v.num / het.1.v.denom
het.1.num <- het.1.o - het.1.e
het.1.psi <- exp(het.1.num/het.1.v)
het.1.psi



het.2.o <- het.OR.a.2
het.2.e <- ((het.OR.a.2 + het.OR.b.2)*(het.OR.a.2 + het.OR.c.2)) / het.OR.N2
het.2.v.num <- (het.OR.a.2 + het.OR.b.2)*(het.OR.c.2 + het.OR.d.2)*
  (het.OR.a.2 + het.OR.c.2)*(het.OR.b.2 + het.OR.d.2)
het.2.v.denom <- (het.OR.N2^2)*(het.OR.N2-1)
het.2.v <- het.2.v.num / het.2.v.denom
het.2.num <- het.2.o - het.2.e
het.2.psi <- exp(het.2.num/het.2.v)
het.2.psi

#Pooled estimate 

het.peto <- exp((sum(het.1.num, het.2.num)/ sum(het.1.v, het.2.v)))
het.peto

#Using R Function Peto
library(metafor)

het.peto.fxn <- rma.peto(c(het.OR.a.1, het.OR.a.2), 
                         c(het.OR.b.1, het.OR.b.2), 
                         c(het.OR.c.1, het.OR.c.2),
                         c(het.OR.d.1, het.OR.d.2),
                         het.OR.N1, het.OR.N2)


OR.het.peto.var <- het.peto.fxn$vb
OR.het.peto.var


#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

#het RR 

#Subgroup 1
het.RR.1 <- matrix(nrow=3, ncol=3)

het.vn.RR.1 <- c(42,58,60,40) # VectRR of cell values c(a,b,c,d)

het.RR.a.1 <-het.RR.1[1,1]<- het.vn.RR.1[1] #Cell a
het.RR.b.1 <- het.RR.1[1,2] <- het.vn.RR.1[2] #Cell b
het.RR.c.1 <- het.RR.1[2,1] <- het.vn.RR.1[3] #Cell c
het.RR.d.1 <- het.RR.1[2,2]<- het.vn.RR.1[4] #Cell d
het.RR.treat.n1<- het.RR.1[1,3] <- het.RR.a.1 + het.RR.b.1 #n Treatment
het.RR.cont.n1 <- het.RR.1[2,3] <- het.RR.c.1 + het.RR.d.1 #n Control
het.RR.e.n1 <- het.RR.1[3,1] <- het.RR.a.1 + het.RR.c.1 #n Event
het.RR.ne.n1 <- het.RR.1[3,2] <- het.RR.b.1 + het.RR.d.1 #n No Event
het.RR.N1 <- het.RR.1[3,3] <- het.RR.a.1 + het.RR.b.1 + het.RR.c.1 + het.RR.d.1 #Total Sub1 

#Subgroup 2
het.RR.2 <- matrix(nrow=3, ncol=3)

het.vn.RR.2 <- c(40,60,20,80) # VectRR of cell values c(a,b,c,d)

het.RR.a.2 <-het.RR.2[1,1]<- het.vn.RR.2[1] #Cell a
het.RR.b.2 <- het.RR.2[1,2] <- het.vn.RR.2[2] #Cell b
het.RR.c.2 <- het.RR.2[2,1] <- het.vn.RR.2[3] #Cell c
het.RR.d.2 <- het.RR.2[2,2]<- het.vn.RR.2[4] #Cell d
het.RR.treat.n2<- het.RR.2[1,3] <- het.RR.a.2 + het.RR.b.2 #n Treatment
het.RR.cont.n2 <- het.RR.2[2,3] <- het.RR.c.2 + het.RR.d.2 #n Control
het.RR.e.n2 <- het.RR.2[3,1] <- het.RR.a.2 + het.RR.c.2 #n Event
het.RR.ne.n2 <- het.RR.2[3,2] <- het.RR.b.2 + het.RR.d.2 #n No Event
het.RR.N2 <- het.RR.2[3,3] <- het.RR.a.2 + het.RR.b.2 + het.RR.c.2 + het.RR.d.2 #Total Sub2


#Collapsed 

het.RR.c <- matrix(nrow=3, ncol=3)

het.vn.RR.c <- c(het.RR.a.1+het.RR.a.2,het.RR.b.1+het.RR.b.2,
                 het.RR.c.1+het.RR.c.2,het.RR.d.1+het.RR.d.2) # VectRR of cell values c(a,b,c,d)

het.RR.a.c <-het.RR.c[1,1]<- het.vn.RR.c[1] #Cell a
het.RR.b.c <- het.RR.c[1,2] <- het.vn.RR.c[2] #Cell b
het.RR.c.c <- het.RR.c[2,1] <- het.vn.RR.c[3] #Cell c
het.RR.d.c <- het.RR.c[2,2]<- het.vn.RR.c[4] #Cell d
het.RR.treat.nc<- het.RR.c[1,3] <- het.RR.a.c + het.RR.b.c #n Treatment
het.RR.cont.nc <- het.RR.c[2,3] <- het.RR.c.c + het.RR.d.c #n Control
het.RR.e.nc <- het.RR.c[3,1] <- het.RR.a.c + het.RR.c.c #n Event
het.RR.ne.nc <- het.RR.c[3,2] <- het.RR.b.c + het.RR.d.c #n No Event
het.RR.Nc <- het.RR.c[3,3] <- het.RR.a.c + het.RR.b.c + het.RR.c.c + het.RR.d.c #Total Collapsed

#RR calculations 

RR.het.1 <- (het.RR.a.1/(het.RR.treat.n1)) / (het.RR.c.1/(het.RR.cont.n1))
RR.het.2 <- (het.RR.a.2/(het.RR.treat.n2)) / (het.RR.c.2/(het.RR.cont.n2))
RR.het.c <- (het.RR.a.c/(het.RR.treat.nc)) / (het.RR.c.c/(het.RR.cont.nc))

RR.het.c
#RR 95% CI Calculations

#RR 95% CI
RR.het.1.95.int <- (het.RR.b.1/het.RR.a.1)/het.RR.treat.n1 + 
  (het.RR.d.1/het.RR.c.1)/het.RR.cont.n1
RR.het.2.95.int <- (het.RR.b.2/het.RR.a.2)/het.RR.treat.n2 + 
  (het.RR.d.2/het.RR.c.2)/het.RR.cont.n2
RR.het.c.95.int <- (het.RR.b.c/het.RR.a.c)/het.RR.treat.nc + 
  (het.RR.d.c/het.RR.c.c)/het.RR.cont.nc


RR.het.1.95.l <- exp(log(RR.het.1) - 1.96*sqrt(RR.het.1.95.int))
RR.het.1.95.u <- exp(log(RR.het.1) + 1.96*sqrt(RR.het.1.95.int))

RR.het.2.95.l <- exp(log(RR.het.2) - 1.96*sqrt(RR.het.2.95.int))
RR.het.2.95.u <- exp(log(RR.het.2) + 1.96*sqrt(RR.het.2.95.int))

RR.het.c.95.l <- exp(log(RR.het.c) - 1.96*sqrt(RR.het.c.95.int))
RR.het.c.95.u <- exp(log(RR.het.c) + 1.96*sqrt(RR.het.c.95.int))


RR.het.1.95.CI <- c(RR.het.1.95.l, RR.het.1.95.u)
RR.het.2.95.CI <- c(RR.het.2.95.l, RR.het.2.95.u)
RR.het.c.95.CI <- c(RR.het.c.95.l, RR.het.c.95.u)

RR.het.1.95.CI
RR.het.2.95.CI
RR.het.c.95.CI


#Using ESCALC Function RR
RR.het.1.var.int <-escalc("RR", het.RR.a.1, het.RR.b.1, het.RR.c.1, het.RR.d.1)
RR.het.2.var.int <-escalc("RR", het.RR.a.2, het.RR.b.2, het.RR.c.2, het.RR.d.2)
RR.het.c.var.int <-escalc("RR", het.RR.a.c, het.RR.b.c, het.RR.c.c, het.RR.d.c)

RR.het.1.var <- RR.het.1.var.int$vi
RR.het.2.var <- RR.het.2.var.int$vi
RR.het.c.var <- RR.het.c.var.int$vi

qq<-escalc("RR", het.RR.a.1, het.RR.b.1, het.RR.c.1, het.RR.d.1)


RR.het.1.var
RR.het.2.var
RR.het.c.var

#RR MH Calculations 

het.numerator.1.RR <- (het.RR.a.1*(het.RR.c.1+het.RR.d.1)) / het.RR.N1
het.denom.1.RR <-  (het.RR.c.1*(het.RR.a.1+het.RR.b.1)) / het.RR.N1
het.numerator.2.RR <- (het.RR.a.2*(het.RR.c.2+het.RR.d.2)) / het.RR.N2
het.denom.2.RR <- (het.RR.c.2*(het.RR.a.2+het.RR.b.2)) / het.RR.N2
het.RR.MH <- sum(het.numerator.1.RR,het.numerator.2.RR) / 
  sum(het.denom.1.RR,het.denom.2.RR)

het.RR.MH
#Using R Function RR

library(metafor)

het.fxn.MH.RR.c<-rma.mh(measure="RR", c(het.RR.a.1, het.RR.a.2),
                        c(het.RR.b.1, het.RR.b.2),
                        c(het.RR.c.1, het.RR.c.2),
                        c(het.RR.d.1, het.RR.d.2),
                        c(het.RR.treat.n1, het.RR.treat.n2),
                        c(het.RR.cont.n1, het.RR.cont.n2),
                        c(het.RR.e.n1, het.RR.e.n2),
                        c(het.RR.ne.n1, het.RR.ne.n2))
het.fxn.MH.RR.c

RR.het.MH.var <- het.fxn.MH.RR.c$vb
RR.het.MH.var
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

#het RD 

#Subgroup 1
het.RD.1 <- matrix(nrow=3, ncol=3)

het.vn.RD.1 <- c(60,40,30,70) # VectRD of cell values c(a,b,c,d)

het.RD.a.1 <-het.RD.1[1,1]<- het.vn.RD.1[1] #Cell a
het.RD.b.1 <- het.RD.1[1,2] <- het.vn.RD.1[2] #Cell b
het.RD.c.1 <- het.RD.1[2,1] <- het.vn.RD.1[3] #Cell c
het.RD.d.1 <- het.RD.1[2,2]<- het.vn.RD.1[4] #Cell d
het.RD.treat.n1<- het.RD.1[1,3] <- het.RD.a.1 + het.RD.b.1 #n Treatment
het.RD.cont.n1 <- het.RD.1[2,3] <- het.RD.c.1 + het.RD.d.1 #n Control
het.RD.e.n1 <- het.RD.1[3,1] <- het.RD.a.1 + het.RD.c.1 #n Event
het.RD.ne.n1 <- het.RD.1[3,2] <- het.RD.b.1 + het.RD.d.1 #n No Event
het.RD.N1 <- het.RD.1[3,3] <- het.RD.a.1 + het.RD.b.1 + het.RD.c.1 + het.RD.d.1 #Total Sub1 

#Subgroup 2
het.RD.2 <- matrix(nrow=3, ncol=3)

het.vn.RD.2 <- c(40,60,90,10) # VectRD of cell values c(a,b,c,d)

het.RD.a.2 <-het.RD.2[1,1]<- het.vn.RD.2[1] #Cell a
het.RD.b.2 <- het.RD.2[1,2] <- het.vn.RD.2[2] #Cell b
het.RD.c.2 <- het.RD.2[2,1] <- het.vn.RD.2[3] #Cell c
het.RD.d.2 <- het.RD.2[2,2]<- het.vn.RD.2[4] #Cell d
het.RD.treat.n2<- het.RD.2[1,3] <- het.RD.a.2 + het.RD.b.2 #n Treatment
het.RD.cont.n2 <- het.RD.2[2,3] <- het.RD.c.2 + het.RD.d.2 #n Control
het.RD.e.n2 <- het.RD.2[3,1] <- het.RD.a.2 + het.RD.c.2 #n Event
het.RD.ne.n2 <- het.RD.2[3,2] <- het.RD.b.2 + het.RD.d.2 #n No Event
het.RD.N2 <- het.RD.2[3,3] <- het.RD.a.2 + het.RD.b.2 + het.RD.c.2 + het.RD.d.2 #Total Sub2


#Collapsed 

het.RD.c <- matrix(nrow=3, ncol=3)

het.vn.RD.c <- c(het.RD.a.1+het.RD.a.2,het.RD.b.1+het.RD.b.2,
                 het.RD.c.1+het.RD.c.2,het.RD.d.1+het.RD.d.2) # VectRD of cell values c(a,b,c,d)

het.RD.a.c <-het.RD.c[1,1]<- het.vn.RD.c[1] #Cell a
het.RD.b.c <- het.RD.c[1,2] <- het.vn.RD.c[2] #Cell b
het.RD.c.c <- het.RD.c[2,1] <- het.vn.RD.c[3] #Cell c
het.RD.d.c <- het.RD.c[2,2]<- het.vn.RD.c[4] #Cell d
het.RD.treat.nc<- het.RD.c[1,3] <- het.RD.a.c + het.RD.b.c #n Treatment
het.RD.cont.nc <- het.RD.c[2,3] <- het.RD.c.c + het.RD.d.c #n Control
het.RD.e.nc <- het.RD.c[3,1] <- het.RD.a.c + het.RD.c.c #n Event
het.RD.ne.nc <- het.RD.c[3,2] <- het.RD.b.c + het.RD.d.c #n No Event
het.RD.Nc <- het.RD.c[3,3] <- het.RD.a.c + het.RD.b.c + het.RD.c.c + het.RD.d.c #Total Collapsed


#RD calculations 

RD.het.1 <- (het.RD.a.1/(het.RD.treat.n1)) - (het.RD.c.1/(het.RD.cont.n1))
RD.het.2 <- (het.RD.a.2/(het.RD.treat.n2)) - (het.RD.c.2/(het.RD.cont.n2))
RD.het.c <- (het.RD.a.c/het.RD.treat.nc) - (het.RD.c.c/het.RD.cont.nc)

RD.het.1
#Risk difference 95% CI 

RD.het.1.se <- ((het.RD.a.1 * het.RD.b.1) / ((het.OR.treat.n1)^3)) + ((het.RD.c.1 * het.RD.d.1) / ((het.OR.cont.n1)^3))
RD.het.2.se <- ((het.RD.a.2 * het.RD.b.2) / ((het.OR.treat.n2)^3)) + ((het.RD.c.2 * het.RD.d.2) / ((het.OR.cont.n2)^3))
RD.het.c.se <- ((het.RD.a.c * het.RD.b.c) / ((het.OR.treat.nc)^3)) + ((het.RD.c.c * het.RD.d.c) / ((het.OR.cont.nc)^3))

RD.het.1.95.l <- RD.het.1 - 1.96*sqrt(RD.het.1.se)
RD.het.1.95.u <- RD.het.1 + 1.96*sqrt(RD.het.1.se)

RD.het.2.95.l <- RD.het.2 - 1.96*sqrt(RD.het.2.se)
RD.het.2.95.u <- RD.het.2 + 1.96*sqrt(RD.het.2.se)

RD.het.c.95.l <- RD.het.c - 1.96*sqrt(RD.het.c.se)
RD.het.c.95.u <- RD.het.c + 1.96*sqrt(RD.het.c.se)

RD.het.1.95 <- c(RD.het.1.95.l, RD.het.1.95.u)
RD.het.2.95 <- c(RD.het.2.95.l, RD.het.2.95.u)
RD.het.c.95 <- c(RD.het.c.95.l, RD.het.c.95.u)

RD.het.1.95
RD.het.2.95
RD.het.c.95

#Using ESCALC Function RD
RD.het.1.var.int <-escalc("RD", het.RD.a.1, het.RD.b.1, het.RD.c.1, het.RD.d.1)
RD.het.2.var.int <-escalc("RD", het.RD.a.2, het.RD.b.2, het.RD.c.2, het.RD.d.2)
RD.het.c.var.int <-escalc("RD", het.RD.a.c, het.RD.b.c, het.RD.c.c, het.RD.d.c)

RD.het.1.var <- RD.het.1.var.int$vi
RD.het.2.var <- RD.het.2.var.int$vi
RD.het.c.var <- RD.het.c.var.int$vi

RD.het.1.var
RD.het.2.var
RD.het.c.var

#MH RD calculations 

het.num.1.RD <- (het.RD.a.1*((het.RD.b.1+het.RD.d.1)/het.RD.N1))
het.denom.1.RD <- ((het.RD.a.1 + het.RD.c.1)*(het.RD.b.1+het.RD.d.1)) / het.RD.N1
het.num.2.RD <- (het.RD.a.2*((het.RD.b.2+het.RD.d.2)/het.RD.N2))
het.denom.2.RD <- ((het.RD.a.2 + het.RD.c.2)*(het.RD.b.2+het.RD.d.2)) / het.RD.N2
het.RD.MH <- ((sum(het.num.1.RD, het.num.2.RD) - 
                 (het.RD.b.1*((het.RD.a.1 + het.RD.c.1)/het.RD.N1)) -
                 (het.RD.b.2*((het.RD.a.2 + het.RD.c.2)/het.RD.N2)))
              / (sum(het.denom.1.RD, het.denom.2.RD)))

het.RD.MH

#Using R Function MH RD

library(metafor)

het.fxn.MH.RD.c<-rma.mh(measure="RD", c(het.RD.a.1, het.RD.a.2),
                        c(het.RD.b.1, het.RD.b.2),
                        c(het.RD.c.1, het.RD.c.2),
                        c(het.RD.d.1, het.RD.d.2),
                        c(het.RD.treat.n1, het.RD.treat.n2),
                        c(het.RD.cont.n1, het.RD.cont.n2),
                        c(het.RD.e.n1, het.RD.e.n2),
                        c(het.RD.ne.n1, het.RD.ne.n2))

het.fxn.MH.RD.c

RD.het.MH.var <- het.fxn.MH.RD.c$vb
RD.het.MH.var

#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

#Using common effects models

#Calculating OR 

OR.het.1.weight <- 1/OR.het.1.var
OR.het.2.weight <- 1/OR.het.2.var

#Weighted mean (i.e., Common Effect MD )  
het.num.1.OR <- OR.het.1.weight * OR.het.1
het.num.2.OR<- OR.het.2.weight * OR.het.2
het.CE.OR<- (het.num.1.OR + het.num.2.OR)/ 
  (OR.het.1.weight + OR.het.2.weight)
het.CE.OR

#Variance of CE OR estimate
het.CE.OR.var <- 1 / (OR.het.1.weight+ OR.het.2.weight)

#Calculating RR 

RR.het.1.weight <- 1/RR.het.1.var
RR.het.2.weight <- 1/RR.het.2.var

#Weighted mean (i.e., Common Effect MD )  
het.num.1.RR <- RR.het.1.weight * RR.het.1
het.num.2.RR<- RR.het.2.weight * RR.het.2
het.CE.RR<- (het.num.1.RR + het.num.2.RR)/ 
  (RR.het.1.weight + RR.het.2.weight)

het.CE.RR

#Variance of CE RR estimate
het.CE.RR.var <- 1 / (RR.het.1.weight+ RR.het.2.weight)

het.CE.RR 
het.CE.RR.var

#Calculating RD

RD.het.1.weight <- 1/RD.het.1.var
RD.het.2.weight <- 1/RD.het.2.var

#Weighted mean (i.e., Common Effect MD )  
het.num.1.RD <- RD.het.1.weight * RD.het.1
het.num.2.RD<- RD.het.2.weight * RD.het.2
het.CE.RD<- (het.num.1.RD + het.num.2.RD)/ 
  (RD.het.1.weight + RD.het.2.weight)

het.CE.RD

#Variance of CE RD estimate
het.CE.RD.var <- 1 / (RD.het.1.weight+ RD.het.2.weight)

het.CE.RD
het.CE.RD.var

#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

#Random effects model 

#For OR 

#Finding weights 

tau <- 0.3

OR.het.1.v.weight <- OR.het.1.var + tau
OR.het.2.v.weight <- OR.het.2.var + tau

OR.het.1.re.weight <- 1 / OR.het.1.v.weight
OR.het.2.re.weight <- 1 / OR.het.2.v.weight

#RE Effect size OR

OR.het.1.re.num <- OR.het.1.re.weight*OR.het.1
OR.het.2.re.num <- OR.het.2.re.weight*OR.het.2
OR.het.re <- (OR.het.1.re.num + OR.het.2.re.num) / 
            (OR.het.1.v.weight + OR.het.2.v.weight)

OR.het.re

###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
#Common Effects Combining in study 

#OR
CE.OR.comb<-rma(yi= c(log(OR.het.1), log(OR.het.2)),
                vi = c(OR.het.1.var, OR.het.2.var),
                method="FE")
exp(CE.OR.comb$b)
c(exp(CE.OR.comb$ci.lb),exp(CE.OR.comb$ci.ub) )

#RR
CE.RR.comb<-rma(yi= c(log(RR.het.1), log(RR.het.2)),
                vi = c(RR.het.1.var, RR.het.2.var),
                method="FE")
exp(CE.RR.comb$b)
c(exp(CE.RR.comb$ci.lb),exp(CE.RR.comb$ci.ub) )

#RR
CE.RD.comb<-rma(yi= c(RD.het.1, RD.het.2),
                vi = c(RD.het.1.var, RD.het.2.var),
                method="FE")
CE.RD.comb


#Random Effects Combining in study 


#OR
RE.OR.comb<-rma(yi= c(log(OR.het.1), log(OR.het.2)),
                vi = c(OR.het.1.var, OR.het.2.var))
exp(RE.OR.comb$b)
c(exp(RE.OR.comb$ci.lb),exp(RE.OR.comb$ci.ub) )
RE.OR.comb

het.RE.OR <- exp(RE.OR.comb$b)
het.RE.OR.var <- RE.OR.comb$vb


#RR
RE.RR.comb<-rma(yi= c(log(RR.het.1), log(RR.het.2)),
                vi = c(RR.het.1.var, RR.het.2.var))
exp(RE.RR.comb$b)
c(exp(RE.RR.comb$ci.lb),exp(CE.RR.comb$ci.ub) )
RE.RR.comb

het.RE.RR <- exp(RE.RR.comb$b)
het.RE.RR.var <- RE.RR.comb$vb

het.RE.RR
het.RE.RR.var

#RD
RE.RD.comb<-rma(yi= c(RD.het.1, RD.het.2),
                vi = c(RD.het.1.var, RD.het.2.var))
RE.RD.comb

het.RE.RD <- RE.RD.comb$b
het.RE.RD.var <-RE.RD.comb$vb
