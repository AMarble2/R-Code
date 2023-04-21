library(metafor)
#Homogeneous OR 

#optimizing OR Cells for OR = 2
x <- seq(1,100,by=1)
e <- matrix(nrow=100, ncol=4)
for (i in 1:100) {
  a <- x[i]
  b <- 100-a
  c <- - (100*a)/(a-200)
  d <- (200*(a-100))/(a-200)
  e[i,1:4] <- c(a,b,c,d)
}

e

#Homogeneous OR 

#Subgroup 1
hom.OR.1 <- matrix(nrow=3, ncol=3)

hom.vn.OR.1 <- c(75,25,60,40) # Vector of cell values c(a,b,c,d)

hom.OR.a.1 <-hom.OR.1[1,1]<- hom.vn.OR.1[1] #Cell a
hom.OR.b.1 <- hom.OR.1[1,2] <- hom.vn.OR.1[2] #Cell b
hom.OR.c.1 <- hom.OR.1[2,1] <- hom.vn.OR.1[3] #Cell c
hom.OR.d.1 <- hom.OR.1[2,2]<- hom.vn.OR.1[4] #Cell d
hom.OR.treat.n1<- hom.OR.1[1,3] <- hom.OR.a.1 + hom.OR.b.1 #n Treatment
hom.OR.cont.n1 <- hom.OR.1[2,3] <- hom.OR.c.1 + hom.OR.d.1 #n Control
hom.OR.e.n1 <- hom.OR.1[3,1] <- hom.OR.a.1 + hom.OR.c.1 #n Event
hom.OR.ne.n1 <- hom.OR.1[3,2] <- hom.OR.b.1 + hom.OR.d.1 #n No Event
hom.OR.N1 <- hom.OR.1[3,3] <- hom.OR.a.1 + hom.OR.b.1 + hom.OR.c.1 + hom.OR.d.1 #Total Sub1 

#Subgroup 2
hom.OR.2 <- matrix(nrow=3, ncol=3)

hom.vn.OR.2 <- c(40,60,25,75) # Vector of cell values c(a,b,c,d)

hom.OR.a.2 <-hom.OR.2[1,1]<- hom.vn.OR.2[1] #Cell a
hom.OR.b.2 <- hom.OR.2[1,2] <- hom.vn.OR.2[2] #Cell b
hom.OR.c.2 <- hom.OR.2[2,1] <- hom.vn.OR.2[3] #Cell c
hom.OR.d.2 <- hom.OR.2[2,2]<- hom.vn.OR.2[4] #Cell d
hom.OR.treat.n2<- hom.OR.2[1,3] <- hom.OR.a.2 + hom.OR.b.2 #n Treatment
hom.OR.cont.n2 <- hom.OR.2[2,3] <- hom.OR.c.2 + hom.OR.d.2 #n Control
hom.OR.e.n2 <- hom.OR.2[3,1] <- hom.OR.a.2 + hom.OR.c.2 #n Event
hom.OR.ne.n2 <- hom.OR.2[3,2] <- hom.OR.b.2 + hom.OR.d.2 #n No Event
hom.OR.N2 <- hom.OR.2[3,3] <- hom.OR.a.2 + hom.OR.b.2 + hom.OR.c.2 + hom.OR.d.2 #Total Sub2


#Collapsed 

hom.OR.c <- matrix(nrow=3, ncol=3)

hom.vn.OR.c <- c(hom.OR.a.1+hom.OR.a.2,hom.OR.b.1+hom.OR.b.2,
                 hom.OR.c.1+hom.OR.c.2,hom.OR.d.1+hom.OR.d.2) # Vector of cell values c(a,b,c,d)

hom.OR.a.c <-hom.OR.c[1,1]<- hom.vn.OR.c[1] #Cell a
hom.OR.b.c <- hom.OR.c[1,2] <- hom.vn.OR.c[2] #Cell b
hom.OR.c.c <- hom.OR.c[2,1] <- hom.vn.OR.c[3] #Cell c
hom.OR.d.c <- hom.OR.c[2,2]<- hom.vn.OR.c[4] #Cell d
hom.OR.treat.nc<- hom.OR.c[1,3] <- hom.OR.a.c + hom.OR.b.c #n Treatment
hom.OR.cont.nc <- hom.OR.c[2,3] <- hom.OR.c.c + hom.OR.d.c #n Control
hom.OR.e.nc <- hom.OR.c[3,1] <- hom.OR.a.c + hom.OR.c.c #n Event
hom.OR.ne.nc <- hom.OR.c[3,2] <- hom.OR.b.c + hom.OR.d.c #n No Event
hom.OR.Nc <- hom.OR.c[3,3] <- hom.OR.a.c + hom.OR.b.c + hom.OR.c.c + hom.OR.d.c #Total Collapsed


hom.OR.a.c
hom.OR.b.c
hom.OR.c.c
hom.OR.d.c


#OR Calculations

OR.hom.1 <- (hom.OR.a.1*hom.OR.d.1) / (hom.OR.b.1*hom.OR.c.1)
OR.hom.2 <- (hom.OR.a.2*hom.OR.d.2) / (hom.OR.b.2*hom.OR.c.2)
OR.hom.c <- (hom.OR.a.c*hom.OR.d.c) / (hom.OR.b.c*hom.OR.c.c) 

OR.hom.c

#OR Variance calculations

OR.hom.1.var.calc <- 1/hom.OR.a.1 + 1/hom.OR.b.1 + 1/hom.OR.c.1 + 1/hom.OR.d.1
OR.hom.2.var.calc <- 1/hom.OR.a.2 + 1/hom.OR.b.2 + 1/hom.OR.c.2 + 1/hom.OR.d.2
OR.hom.c.var.calc <- 1/hom.OR.a.c + 1/hom.OR.b.c + 1/hom.OR.c.c + 1/hom.OR.d.c

OR.hom.c.var.calc

log(OR.hom.1)

#OR 95% CI calculations 
OR.hom.1.95.n <- log(OR.hom.1) - 1.96*sqrt(OR.hom.1.var.calc)
OR.hom.1.95.p <- log(OR.hom.1) + 1.96*sqrt(OR.hom.1.var.calc)

OR.hom.2.95.n <- log(OR.hom.2) - 1.96*sqrt(OR.hom.2.var.calc)
OR.hom.2.95.p <- log(OR.hom.2) + 1.96*sqrt(OR.hom.2.var.calc)

OR.hom.c.95.n <- log(OR.hom.c) - 1.96*sqrt(OR.hom.c.var.calc)
OR.hom.c.95.p <- log(OR.hom.c) + 1.96*sqrt(OR.hom.c.var.calc)

OR.hom.1.CI<-c(exp(OR.hom.1.95.n), exp(OR.hom.1.95.p))
OR.hom.2.CI<-c(exp(OR.hom.2.95.n), exp(OR.hom.2.95.p))
OR.hom.c.CI<-c(exp(OR.hom.c.95.n), exp(OR.hom.c.95.p))

OR.hom.1.CI
OR.hom.2.CI
OR.hom.c.CI

#Using ESCALC OR 

OR.hom.1.var.int <-escalc("OR", hom.OR.a.1, hom.OR.b.1, hom.OR.c.1, hom.OR.d.1)
OR.hom.2.var.int <-escalc("OR", hom.OR.a.2, hom.OR.b.2, hom.OR.c.2, hom.OR.d.2)
OR.hom.c.var.int <-escalc("OR", hom.OR.a.c, hom.OR.b.c, hom.OR.c.c, hom.OR.d.c)

OR.hom.1.var <- OR.hom.1.var.int$vi
OR.hom.2.var <- OR.hom.2.var.int$vi
OR.hom.c.var <- OR.hom.c.var.int$vi

OR.hom.1.var
OR.hom.2.var
OR.hom.c.var

escalc("OR", hom.OR.a.1, hom.OR.b.1, hom.OR.c.1, hom.OR.d.1)

#OR MH Calculations 

hom.numerator.1.MH.OR <- (hom.OR.a.1*hom.OR.d.1) / hom.OR.N1
hom.denom.1.MH.OR <- (hom.OR.b.1*hom.OR.c.1) / hom.OR.N1
hom.numerator.2.MH.OR <- (hom.OR.a.2*hom.OR.d.2) / hom.OR.N2
hom.denom.2.MH.OR <- (hom.OR.b.2*hom.OR.c.2) / hom.OR.N2
OR.hom.MH <- sum(hom.numerator.1.MH.OR,hom.numerator.2.MH.OR) / 
             sum(hom.denom.1.MH.OR,hom.denom.2.MH.OR)

#Variance estimator for OR MH 

hom.P1 <- (hom.OR.a.1+hom.OR.d.1)/n
hom.Q1 <- (hom.OR.b.1+hom.OR.c.1)/n
hom.R1 <- (hom.OR.a.1*hom.OR.d.1)/n
hom.S1 <- (hom.OR.b.1*hom.OR.c.1)/n
hom.R1P1 <- hom.R1*hom.P1
hom.P1S1 <- hom.P1*hom.S1
hom.Q1R1 <- hom.Q1*hom.R1
hom.S1Q1 <- hom.S1*hom.Q1
hom.P2 <- (hom.OR.a.2+hom.OR.d.2)/n
hom.Q2 <- (hom.OR.b.2+hom.OR.c.2)/n
hom.R2 <- (hom.OR.a.2*hom.OR.d.2)/n
hom.S2 <- (hom.OR.b.2*hom.OR.c.2)/n
hom.R2P2 <- hom.R2*hom.P2
hom.P2S2 <- hom.P2*hom.S2
hom.Q2R2 <- hom.Q2*hom.R2
hom.S2Q2 <- hom.S2*hom.Q2

hom.num.MH.1 <- sum(hom.R1P1, hom.R2P2)
hom.num.MH.2 <- sum(hom.P1S1+hom.Q1R1, hom.P2S2+hom.Q2R2)
hom.num.MH.3 <- sum(hom.S1Q1, hom.S2Q2)
hom.den.MH.1 <- 2*((sum(hom.R1, hom.R2))^2)
hom.den.MH.2 <- 2*((sum(hom.R1,hom.R2))*(sum(hom.S1,hom.S2)))
hom.den.MH.3 <- 2*((sum(hom.R1,hom.R2))*(sum(hom.S1,hom.S2)^2))

OR.hom.MH.var <- hom.num.MH.1/hom.den.MH.1 +
                 hom.num.MH.2/hom.den.MH.2+
                 hom.num.MH.2/hom.den.MH.2
                   

OR.hom.MH.var


#Using R Function MH OR
library(metafor)

hom.fxn.MH.OR.c<-rma.mh(measure="OR", c(hom.OR.a.1, hom.OR.a.2),
                 c(hom.OR.b.1, hom.OR.b.2),
                 c(hom.OR.c.1, hom.OR.c.2),
                 c(hom.OR.d.1, hom.OR.d.2),
                 c(hom.OR.treat.n1, hom.OR.treat.n2),
                 c(hom.OR.cont.n1, hom.OR.cont.n2),
                 c(hom.OR.e.n1, hom.OR.e.n2),
                 c(hom.OR.ne.n1, hom.OR.ne.n2))

hom.fxn.MH.OR.c$vb
OR.hom.MH.var





#Peto OR Calculation

#Subgroup 1
hom.1.o <- hom.OR.a.1
hom.1.e <- ((hom.OR.a.1 + hom.OR.b.1)*(hom.OR.a.1 + hom.OR.c.1)) / hom.OR.N1
hom.1.v.num <- (hom.OR.a.1 + hom.OR.b.1)*(hom.OR.c.1 + hom.OR.d.1)*
               (hom.OR.a.1 + hom.OR.c.1)*(hom.OR.b.1 + hom.OR.d.1)
hom.1.v.denom <- (hom.OR.N1^2)*(hom.OR.N1-1)
hom.1.v <- hom.1.v.num / hom.1.v.denom
hom.1.num <- hom.1.o - hom.1.e
hom.1.psi <- exp(hom.1.num/hom.1.v)
hom.1.psi


hom.2.o <- hom.OR.a.2
hom.2.e <- ((hom.OR.a.2 + hom.OR.b.2)*(hom.OR.a.2 + hom.OR.c.2)) / hom.OR.N2
hom.2.v.num <- (hom.OR.a.2 + hom.OR.b.2)*(hom.OR.c.2 + hom.OR.d.2)*
  (hom.OR.a.2 + hom.OR.c.2)*(hom.OR.b.2 + hom.OR.d.2)
hom.2.v.denom <- (hom.OR.N2^2)*(hom.OR.N2-1)
hom.2.v <- hom.2.v.num / hom.2.v.denom
hom.2.num <- hom.2.o - hom.2.e
hom.2.psi <- exp(hom.2.num/hom.2.v)
hom.2.psi

#Pooled estimate 

OR.hom.peto <- exp((sum(hom.1.num, hom.2.num)/ sum(hom.1.v, hom.2.v)))
OR.hom.peto



#Using R Function Peto
library(metafor)

hom.peto.fxn <- rma.peto(c(hom.OR.a.1, hom.OR.a.2), 
                         c(hom.OR.b.1, hom.OR.b.2), 
                         c(hom.OR.c.1, hom.OR.c.2),
                         c(hom.OR.d.1, hom.OR.d.2),
                          hom.OR.N1, hom.OR.N2)
hom.peto.fxn


OR.hom.peto.var <- hom.peto.fxn$vb
OR.hom.peto.var
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

#Hom RR 

#Subgroup 1
hom.RR.1 <- matrix(nrow=3, ncol=3)

hom.vn.RR.1 <- c(30,70,15,85) # VectRR of cell values c(a,b,c,d)

hom.RR.a.1 <-hom.RR.1[1,1]<- hom.vn.RR.1[1] #Cell a
hom.RR.b.1 <- hom.RR.1[1,2] <- hom.vn.RR.1[2] #Cell b
hom.RR.c.1 <- hom.RR.1[2,1] <- hom.vn.RR.1[3] #Cell c
hom.RR.d.1 <- hom.RR.1[2,2]<- hom.vn.RR.1[4] #Cell d
hom.RR.treat.n1<- hom.RR.1[1,3] <- hom.RR.a.1 + hom.RR.b.1 #n Treatment
hom.RR.cont.n1 <- hom.RR.1[2,3] <- hom.RR.c.1 + hom.RR.d.1 #n Control
hom.RR.e.n1 <- hom.RR.1[3,1] <- hom.RR.a.1 + hom.RR.c.1 #n Event
hom.RR.ne.n1 <- hom.RR.1[3,2] <- hom.RR.b.1 + hom.RR.d.1 #n No Event
hom.RR.N1 <- hom.RR.1[3,3] <- hom.RR.a.1 + hom.RR.b.1 + hom.RR.c.1 + hom.RR.d.1 #Total Sub1 

#Subgroup 2
hom.RR.2 <- matrix(nrow=3, ncol=3)

hom.vn.RR.2 <- c(40,60,20,80) # VectRR of cell values c(a,b,c,d)

hom.RR.a.2 <-hom.RR.2[1,1]<- hom.vn.RR.2[1] #Cell a
hom.RR.b.2 <- hom.RR.2[1,2] <- hom.vn.RR.2[2] #Cell b
hom.RR.c.2 <- hom.RR.2[2,1] <- hom.vn.RR.2[3] #Cell c
hom.RR.d.2 <- hom.RR.2[2,2]<- hom.vn.RR.2[4] #Cell d
hom.RR.treat.n2<- hom.RR.2[1,3] <- hom.RR.a.2 + hom.RR.b.2 #n Treatment
hom.RR.cont.n2 <- hom.RR.2[2,3] <- hom.RR.c.2 + hom.RR.d.2 #n Control
hom.RR.e.n2 <- hom.RR.2[3,1] <- hom.RR.a.2 + hom.RR.c.2 #n Event
hom.RR.ne.n2 <- hom.RR.2[3,2] <- hom.RR.b.2 + hom.RR.d.2 #n No Event
hom.RR.N2 <- hom.RR.2[3,3] <- hom.RR.a.2 + hom.RR.b.2 + hom.RR.c.2 + hom.RR.d.2 #Total Sub2


#Collapsed 

hom.RR.c <- matrix(nrow=3, ncol=3)

hom.vn.RR.c <- c(hom.RR.a.1+hom.RR.a.2,hom.RR.b.1+hom.RR.b.2,
                 hom.RR.c.1+hom.RR.c.2,hom.RR.d.1+hom.RR.d.2) # VectRR of cell values c(a,b,c,d)

hom.RR.a.c <-hom.RR.c[1,1]<- hom.vn.RR.c[1] #Cell a
hom.RR.b.c <- hom.RR.c[1,2] <- hom.vn.RR.c[2] #Cell b
hom.RR.c.c <- hom.RR.c[2,1] <- hom.vn.RR.c[3] #Cell c
hom.RR.d.c <- hom.RR.c[2,2]<- hom.vn.RR.c[4] #Cell d
hom.RR.treat.nc<- hom.RR.c[1,3] <- hom.RR.a.c + hom.RR.b.c #n Treatment
hom.RR.cont.nc <- hom.RR.c[2,3] <- hom.RR.c.c + hom.RR.d.c #n Control
hom.RR.e.nc <- hom.RR.c[3,1] <- hom.RR.a.c + hom.RR.c.c #n Event
hom.RR.ne.nc <- hom.RR.c[3,2] <- hom.RR.b.c + hom.RR.d.c #n No Event
hom.RR.Nc <- hom.RR.c[3,3] <- hom.RR.a.c + hom.RR.b.c + hom.RR.c.c + hom.RR.d.c #Total Collapsed

#RR calculations 

RR.hom.1 <- (hom.RR.a.1/(hom.RR.treat.n1)) / (hom.RR.c.1/(hom.RR.cont.n1))
RR.hom.2 <- (hom.RR.a.2/(hom.RR.treat.n2)) / (hom.RR.c.2/(hom.RR.cont.n2))
RR.hom.c <- (hom.RR.a.c/(hom.RR.treat.nc)) / (hom.RR.c.c/(hom.RR.cont.nc))

hom.RR.c

#Using ESCALC Function RR
RR.hom.1.var.int <-escalc("RR", hom.RR.a.1, hom.RR.b.1, hom.RR.c.1, hom.RR.d.1)
RR.hom.2.var.int <-escalc("RR", hom.RR.a.2, hom.RR.b.2, hom.RR.c.2, hom.RR.d.2)
RR.hom.c.var.int <-escalc("RR", hom.RR.a.c, hom.RR.b.c, hom.RR.c.c, hom.RR.d.c)


RR.hom.1.var <- RR.hom.1.var.int$vi
RR.hom.2.var <- RR.hom.2.var.int$vi
RR.hom.c.var <- RR.hom.c.var.int$vi

RR.hom.1.var
RR.hom.2.var
RR.hom.c.var


#RR 95% CI
RR.hom.1.95.int <- (hom.RR.b.1/hom.RR.a.1)/hom.RR.treat.n1 + 
                   (hom.RR.d.1/hom.RR.c.1)/hom.RR.cont.n1
RR.hom.2.95.int <- (hom.RR.b.2/hom.RR.a.2)/hom.RR.treat.n2 + 
                   (hom.RR.d.2/hom.RR.c.2)/hom.RR.cont.n2
RR.hom.c.95.int <- (hom.RR.b.c/hom.RR.a.c)/hom.RR.treat.nc + 
                   (hom.RR.d.c/hom.RR.c.c)/hom.RR.cont.nc


RR.hom.1.95.l <- exp(log(RR.hom.1) - 1.96*sqrt(RR.hom.1.95.int))
RR.hom.1.95.u <- exp(log(RR.hom.1) + 1.96*sqrt(RR.hom.1.95.int))

RR.hom.2.95.l <- exp(log(RR.hom.2) - 1.96*sqrt(RR.hom.2.95.int))
RR.hom.2.95.u <- exp(log(RR.hom.2) + 1.96*sqrt(RR.hom.2.95.int))

RR.hom.c.95.l <- exp(log(RR.hom.c) - 1.96*sqrt(RR.hom.c.95.int))
RR.hom.c.95.u <- exp(log(RR.hom.c) + 1.96*sqrt(RR.hom.c.95.int))


RR.hom.1.95.CI <- c(RR.hom.1.95.l, RR.hom.1.95.u)
RR.hom.2.95.CI <- c(RR.hom.2.95.l, RR.hom.2.95.u)
RR.hom.c.95.CI <- c(RR.hom.c.95.l, RR.hom.c.95.u)

RR.hom.1.95.CI
RR.hom.2.95.CI
RR.hom.c.95.CI


#RR MH Calculations 

hom.numerator.1.RR <- (hom.RR.a.1*(hom.RR.c.1+hom.RR.d.1)) / hom.RR.N1
hom.denom.1.RR <-  (hom.RR.c.1*(hom.RR.a.1+hom.RR.b.1)) / hom.RR.N1
hom.numerator.2.RR <- (hom.RR.a.2*(hom.RR.c.2+hom.RR.d.2)) / hom.RR.N2
hom.denom.2.RR <- (hom.RR.c.2*(hom.RR.a.2+hom.RR.b.2)) / hom.RR.N2
hom.RR.MH <- sum(hom.numerator.1.RR,hom.numerator.2.RR) / 
             sum(hom.denom.1.RR,hom.denom.2.RR)

#Using R Function RR

library(metafor)

hom.fxn.MH.RR.c<-rma.mh(measure="RR", c(hom.RR.a.1, hom.RR.a.2),
                        c(hom.RR.b.1, hom.RR.b.2),
                        c(hom.RR.c.1, hom.RR.c.2),
                        c(hom.RR.d.1, hom.RR.d.2),
                        c(hom.RR.treat.n1, hom.RR.treat.n2),
                        c(hom.RR.cont.n1, hom.RR.cont.n2),
                        c(hom.RR.e.n1, hom.RR.e.n2),
                        c(hom.RR.ne.n1, hom.RR.ne.n2))

hom.fxn.MH.RR.c

RR.hom.MH.var <- hom.fxn.MH.RR.c$vb

RR.hom.MH.var

#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

#Hom RD 

#Subgroup 1
hom.RD.1 <- matrix(nrow=3, ncol=3)

hom.vn.RD.1 <- c(20,80,70,30) # VectRD of cell values c(a,b,c,d)

hom.RD.a.1 <-hom.RD.1[1,1]<- hom.vn.RD.1[1] #Cell a
hom.RD.b.1 <- hom.RD.1[1,2] <- hom.vn.RD.1[2] #Cell b
hom.RD.c.1 <- hom.RD.1[2,1] <- hom.vn.RD.1[3] #Cell c
hom.RD.d.1 <- hom.RD.1[2,2]<- hom.vn.RD.1[4] #Cell d
hom.RD.treat.n1<- hom.RD.1[1,3] <- hom.RD.a.1 + hom.RD.b.1 #n Treatment
hom.RD.cont.n1 <- hom.RD.1[2,3] <- hom.RD.c.1 + hom.RD.d.1 #n Control
hom.RD.e.n1 <- hom.RD.1[3,1] <- hom.RD.a.1 + hom.RD.c.1 #n Event
hom.RD.ne.n1 <- hom.RD.1[3,2] <- hom.RD.b.1 + hom.RD.d.1 #n No Event
hom.RD.N1 <- hom.RD.1[3,3] <- hom.RD.a.1 + hom.RD.b.1 + hom.RD.c.1 + hom.RD.d.1 #Total Sub1 

#Subgroup 2
hom.RD.2 <- matrix(nrow=3, ncol=3)

hom.vn.RD.2 <- c(40,60,90,10) # VectRD of cell values c(a,b,c,d)

hom.RD.a.2 <-hom.RD.2[1,1]<- hom.vn.RD.2[1] #Cell a
hom.RD.b.2 <- hom.RD.2[1,2] <- hom.vn.RD.2[2] #Cell b
hom.RD.c.2 <- hom.RD.2[2,1] <- hom.vn.RD.2[3] #Cell c
hom.RD.d.2 <- hom.RD.2[2,2]<- hom.vn.RD.2[4] #Cell d
hom.RD.treat.n2<- hom.RD.2[1,3] <- hom.RD.a.2 + hom.RD.b.2 #n Treatment
hom.RD.cont.n2 <- hom.RD.2[2,3] <- hom.RD.c.2 + hom.RD.d.2 #n Control
hom.RD.e.n2 <- hom.RD.2[3,1] <- hom.RD.a.2 + hom.RD.c.2 #n Event
hom.RD.ne.n2 <- hom.RD.2[3,2] <- hom.RD.b.2 + hom.RD.d.2 #n No Event
hom.RD.N2 <- hom.RD.2[3,3] <- hom.RD.a.2 + hom.RD.b.2 + hom.RD.c.2 + hom.RD.d.2 #Total Sub2


#Collapsed 

hom.RD.c <- matrix(nrow=3, ncol=3)

hom.vn.RD.c <- c(hom.RD.a.1+hom.RD.a.2,hom.RD.b.1+hom.RD.b.2,
                 hom.RD.c.1+hom.RD.c.2,hom.RD.d.1+hom.RD.d.2) # VectRD of cell values c(a,b,c,d)

hom.RD.a.c <-hom.RD.c[1,1]<- hom.vn.RD.c[1] #Cell a
hom.RD.b.c <- hom.RD.c[1,2] <- hom.vn.RD.c[2] #Cell b
hom.RD.c.c <- hom.RD.c[2,1] <- hom.vn.RD.c[3] #Cell c
hom.RD.d.c <- hom.RD.c[2,2]<- hom.vn.RD.c[4] #Cell d
hom.RD.treat.nc<- hom.RD.c[1,3] <- hom.RD.a.c + hom.RD.b.c #n Treatment
hom.RD.cont.nc <- hom.RD.c[2,3] <- hom.RD.c.c + hom.RD.d.c #n Control
hom.RD.e.nc <- hom.RD.c[3,1] <- hom.RD.a.c + hom.RD.c.c #n Event
hom.RD.ne.nc <- hom.RD.c[3,2] <- hom.RD.b.c + hom.RD.d.c #n No Event
hom.RD.Nc <- hom.RD.c[3,3] <- hom.RD.a.c + hom.RD.b.c + hom.RD.c.c + hom.RD.d.c #Total Collapsed


#RD calculations 

RD.hom.1 <- (hom.RD.a.1/(hom.RD.treat.n1)) - (hom.RD.c.1/(hom.RD.cont.n1))
RD.hom.2 <- (hom.RD.a.2/(hom.RD.treat.n2)) - (hom.RD.c.2/(hom.RD.cont.n2))
RD.hom.c <- (hom.RD.a.c/(hom.RD.treat.nc)) - (hom.RD.c.c/(hom.RD.cont.nc))

#Using ESCALC Function RD
RD.hom.1.var.int <-escalc("RD", hom.RD.a.1, hom.RD.b.1, hom.RD.c.1, hom.RD.d.1)
RD.hom.2.var.int <-escalc("RD", hom.RD.a.2, hom.RD.b.2, hom.RD.c.2, hom.RD.d.2)
RD.hom.c.var.int <-escalc("RD", hom.RD.a.c, hom.RD.b.c, hom.RD.c.c, hom.RD.d.c)

RD.hom.1.var <- RD.hom.1.var.int$vi
RD.hom.2.var <- RD.hom.2.var.int$vi
RD.hom.c.var <- RD.hom.c.var.int$vi

RD.hom.1.var
RD.hom.2.var
RD.hom.c.var

#Risk difference 95% CI 

RD.hom.1.se <- ((hom.RD.a.1 * hom.RD.b.1) / ((hom.OR.treat.n1)^3)) + ((hom.RD.c.1 * hom.RD.d.1) / ((hom.OR.cont.n1)^3))
RD.hom.2.se <- ((hom.RD.a.2 * hom.RD.b.2) / ((hom.OR.treat.n2)^3)) + ((hom.RD.c.2 * hom.RD.d.2) / ((hom.OR.cont.n2)^3))
RD.hom.c.se <- ((hom.RD.a.c * hom.RD.b.c) / ((hom.OR.treat.nc)^3)) + ((hom.RD.c.c * hom.RD.d.c) / ((hom.OR.cont.nc)^3))

RD.hom.1.95.l <- RD.hom.1 - 1.96*sqrt(RD.hom.1.se)
RD.hom.1.95.u <- RD.hom.1 + 1.96*sqrt(RD.hom.1.se)

RD.hom.2.95.l <- RD.hom.2 - 1.96*sqrt(RD.hom.2.se)
RD.hom.2.95.u <- RD.hom.2 + 1.96*sqrt(RD.hom.2.se)

RD.hom.c.95.l <- RD.hom.c - 1.96*sqrt(RD.hom.c.se)
RD.hom.c.95.u <- RD.hom.c + 1.96*sqrt(RD.hom.c.se)

RD.hom.1.95 <- c(RD.hom.1.95.l, RD.hom.1.95.u)
RD.hom.2.95 <- c(RD.hom.2.95.l, RD.hom.2.95.u)
RD.hom.c.95 <- c(RD.hom.c.95.l, RD.hom.c.95.u)

RD.hom.1.95
RD.hom.2.95
RD.hom.c.95


#MH RD calculations 

hom.num.1.RD <- (hom.RD.a.1*((hom.RD.b.1+hom.RD.d.1)/hom.RD.N1))
hom.denom.1.RD <- ((hom.RD.a.1 + hom.RD.c.1)*(hom.RD.b.1+hom.RD.d.1)) / hom.RD.N1
hom.num.2.RD <- (hom.RD.a.2*((hom.RD.b.2+hom.RD.d.2)/hom.RD.N2))
hom.denom.2.RD <- ((hom.RD.a.2 + hom.RD.c.2)*(hom.RD.b.2+hom.RD.d.2)) / hom.RD.N2
hom.RD.MH <- ((sum(hom.num.1.RD, hom.num.2.RD) - 
            (hom.RD.b.1*((hom.RD.a.1 + hom.RD.c.1)/hom.RD.N1)) -
            (hom.RD.b.2*((hom.RD.a.2 + hom.RD.c.2)/hom.RD.N2)))
            / (sum(hom.denom.1.RD, hom.denom.2.RD)))

hom.RD.MH

OR.hom.c.var
OR.hom.MH.var

#Using R Function MH RD

library(metafor)

hom.fxn.MH.RD.c<-rma.mh(measure="RD", c(hom.RD.a.1, hom.RD.a.2),
                        c(hom.RD.b.1, hom.RD.b.2),
                        c(hom.RD.c.1, hom.RD.c.2),
                        c(hom.RD.d.1, hom.RD.d.2),
                        c(hom.RD.treat.n1, hom.RD.treat.n2),
                        c(hom.RD.cont.n1, hom.RD.cont.n2),
                        c(hom.RD.e.n1, hom.RD.e.n2),
                        c(hom.RD.ne.n1, hom.RD.ne.n2))

hom.fxn.MH.RD.c

RD.hom.MH.var <- (hom.RD.MH*hom.fxn.MH.RD.c$se)^2
RD.hom.MH.var
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################


#Using common effects models

#Calculating OR 

OR.hom.1.weight <- 1/OR.hom.1.var
OR.hom.2.weight <- 1/OR.hom.2.var

#Weighted mean (i.e., Common Effect MD )  
hom.num.1.OR <- OR.hom.1.weight * OR.hom.1
hom.num.2.OR<- OR.hom.2.weight * OR.hom.2
hom.CE.OR<- (hom.num.1.OR + hom.num.2.OR)/ 
            (OR.hom.1.weight + OR.hom.2.weight)
hom.CE.OR

#Variance of CE OR estimate
hom.CE.OR.var <- 1 / (OR.hom.1.weight+ OR.hom.2.weight)
hom.CE.OR.var

#Common Effects function OR

rma.uni(yi = c(log(OR.hom.1), log(OR.hom.2)), 
        vi = c(log(OR.hom.1.var), log(OR.hom.1.var))
        , tau2 = 0)
exp(0.6931)

#Calculating RR 

RR.hom.1.weight <- 1/RR.hom.1.var
RR.hom.2.weight <- 1/RR.hom.2.var


#Weighted mean (i.e., Common Effect MD )  
hom.num.1.RR <- RR.hom.1.weight * RR.hom.1
hom.num.2.RR<- RR.hom.2.weight * RR.hom.2
hom.CE.RR<- (hom.num.1.RR + hom.num.2.RR)/ 
  (RR.hom.1.weight + RR.hom.2.weight)

hom.CE.RR

#Variance of CE RR estimate
hom.CE.RR.var <- 1 / (RR.hom.1.weight+ RR.hom.2.weight)

hom.CE.RR
hom.CE.RR.var

#Common Effects function RR

testasdf<-rma.uni(yi = c(log(RR.hom.1), log(RR.hom.2)), 
        vi = c(RR.hom.1.var, RR.hom.2.var),
        method='FE')
exp(0.3393)

testasdf$vb
RR.hom.MH.var
#Calculating RD

RD.hom.1.weight <- 1/RD.hom.1.var
RD.hom.2.weight <- 1/RD.hom.2.var

#Weighted mean (i.e., Common Effect MD )  
hom.num.1.RD <- RD.hom.1.weight * RD.hom.1
hom.num.2.RD<- RD.hom.2.weight * RD.hom.2
hom.CE.RD<- (hom.num.1.RD + hom.num.2.RD)/ 
  (RD.hom.1.weight + RD.hom.2.weight)

hom.CE.RD

#Variance of CE RD estimate
hom.CE.RD.var <- 1 / (RD.hom.1.weight+ RD.hom.2.weight)

hom.CE.RD
hom.CE.RD.var


#Common Effects function RD

rma.uni(yi = c(RD.hom.1, RD.hom.2), 
        vi = c(RD.hom.1.var, RD.hom.1.var)
        , tau2 = 0)

###############################################################################
###############################################################################
###############################################################################
###############################################################################
#Common Effects Combining in study 


#OR
CE.OR.comb<-rma(yi= c(log(OR.hom.1), log(OR.hom.2)),
    vi = c(OR.hom.1.var, OR.hom.2.var),
    method="FE")
exp(CE.OR.comb$b)
c(exp(CE.OR.comb$ci.lb),exp(CE.OR.comb$ci.ub) )

#RR
CE.RR.comb<-rma(yi= c(log(RR.hom.1), log(RR.hom.2)),
                vi = c(RR.hom.1.var, RR.hom.2.var),
                method="FE")
exp(CE.RR.comb$b)
c(exp(CE.RR.comb$ci.lb),exp(CE.RR.comb$ci.ub) )

#RR
CE.RD.comb<-rma(yi= c(RD.hom.1, RD.hom.2),
                vi = c(RD.hom.1.var, RD.hom.2.var),
                method="FE")
CE.RD.comb


#Random Effects Combining in study 


#OR
hom.RE.OR.comb<-rma(yi= c(log(OR.hom.1), log(OR.hom.2)),
                vi = c(OR.hom.1.var, OR.hom.2.var))
hom.RE.OR.comb
exp(RE.OR.comb$b)
c(exp(RE.OR.comb$ci.lb),exp(RE.OR.comb$ci.ub) )
hom.RE.OR.comb.var <- hom.RE.OR.comb$vi
hom.RE.OR.comb.var
#RR
hom.RE.RR.comb<-rma(yi= c(log(RR.hom.1), log(RR.hom.2)),
                vi = c(RR.hom.1.var, RR.hom.2.var))
exp(RE.RR.comb$b)
c(exp(RE.RR.comb$ci.lb),exp(CE.RR.comb$ci.ub) )
hom.RE.RR.comb

#RR
hom.RE.RD.comb<-rma(yi= c(RD.hom.1, RD.hom.2),
                vi = c(RD.hom.1.var, RD.hom.2.var))
hom.RE.RD.comb



