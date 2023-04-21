
library(metafor)
# OR data 

#Studies OR 

s.1.OR <- log(3.07)
s.2.OR <- log(3.47)
s.3.OR <- log(0.15)
s.4.OR <- log(1.55)

s.1.OR.var <- OR.study1.var
s.2.OR.var <- OR.study2.var
s.3.OR.var <- OR.study3.var
s.4.OR.var <- OR.study4.var

#Collapsed OR

OR.Hom.collapse.comb <-rma.uni(yi = c(log(OR.hom.c), s.1.OR, s.2.OR, s.3.OR, s.4.OR),
        vi = c(OR.hom.c.var, s.1.OR.var, s.2.OR.var, s.3.OR.var, s.4.OR.var))
exp(OR.Hom.collapse.comb$b)
c(exp(OR.Hom.collapse.comb$ci.lb),exp(OR.Hom.collapse.comb$ci.ub))
OR.Hom.collapse.comb$tau2



# OR (MH)
OR.Hom.MH.comb <- rma.uni(yi = c(log(OR.hom.MH), s.1.OR, s.2.OR, s.3.OR, s.4.OR),
                 vi = c(OR.hom.MH.var, s.1.OR.var, s.2.OR.var, s.3.OR.var,
                        s.4.OR.var))
exp(OR.Hom.MH.comb$b)
c(exp(OR.Hom.MH.comb$ci.lb),exp(OR.Hom.MH.comb$ci.ub))
OR.Hom.MH.comb$tau2



# OR (Peto)
OR.Hom.peto.comb <- rma.uni(yi = c(log(OR.hom.peto), s.1.OR, s.2.OR, s.3.OR, s.4.OR),
                          vi = c(OR.hom.peto.var, s.1.OR.var, s.2.OR.var, s.3.OR.var,
                                 s.4.OR.var))
exp(OR.Hom.peto.comb$b)
c(exp(OR.Hom.peto.comb$ci.lb),exp(OR.Hom.peto.comb$ci.ub))
OR.Hom.peto.comb$tau2
                        

# OR (Common Effects)
OR.Hom.ce.comb <- rma.uni(yi = c(log(hom.CE.OR), s.1.OR, s.2.OR, s.3.OR, s.4.OR),
                          vi = c(hom.CE.OR.var, s.1.OR.var, s.2.OR.var, s.3.OR.var,
                                 s.4.OR.var))
exp(OR.Hom.ce.comb$b)
c(exp(OR.Hom.ce.comb$ci.lb),exp(OR.Hom.ce.comb$ci.ub))
OR.Hom.ce.comb$tau2

# OR (Random Effects)
OR.Hom.ce.comb <- rma.uni(yi = c(log(hom.CE.OR), s.1.OR, s.2.OR, s.3.OR, s.4.OR),
                          vi = c(hom.CE.OR.var, s.1.OR.var, s.2.OR.var, s.3.OR.var,
                                 s.4.OR.var))
exp(OR.Hom.ce.comb$b)
c(exp(OR.Hom.ce.comb$ci.lb),exp(OR.Hom.ce.comb$ci.ub))
OR.Hom.ce.comb$tau2

# OR (Sep. Entries)
OR.Hom.se.comb <- rma.uni(yi = c(log(OR.hom.1), log(OR.hom.2),
                                 s.1.OR, s.2.OR, s.3.OR, s.4.OR),
                          vi = c(OR.hom.1.var, OR.hom.2.var,
                                 s.1.OR.var, s.2.OR.var, s.3.OR.var,
                                 s.4.OR.var))
exp(OR.Hom.se.comb$b)
c(exp(OR.Hom.se.comb$ci.lb),exp(OR.Hom.se.comb$ci.ub))
OR.Hom.se.comb$tau2


#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

#RR Studies 

s.1.RR <- log(3.77)
s.2.RR <- log(1.04)
s.3.RR <- log(1.04)
s.4.RR <- log(2.08)

s.1.RR.var <- RR.study1.var
s.2.RR.var <- RR.study2.var
s.3.RR.var <- RR.study3.var
s.4.RR.var <- RR.study4.var

#Collapsed RR

RR.Hom.collapse.comb <-rma.uni(yi = c(log(RR.hom.c), s.1.RR, s.2.RR, s.3.RR, s.4.RR),
                               vi = c(RR.hom.c.var, s.1.RR.var, s.2.RR.var, s.3.RR.var, s.4.RR.var))
exp(RR.Hom.collapse.comb$b)
c(exp(RR.Hom.collapse.comb$ci.lb),exp(RR.Hom.collapse.comb$ci.ub))
RR.Hom.collapse.comb$tau2


# RR (MH)
RR.Hom.MH.comb <- rma.uni(yi = c(log(2.00), s.1.RR, s.2.RR, s.3.RR, s.4.RR),
                          vi = c(RR.hom.MH.var, s.1.RR.var, s.2.RR.var, s.3.RR.var,
                                 s.4.RR.var))
exp(RR.Hom.MH.comb$b)
c(exp(RR.Hom.MH.comb$ci.lb),exp(RR.Hom.MH.comb$ci.ub))
RR.Hom.MH.comb$tau2


# RR (Common Effects)
RR.Hom.ce.comb <- rma.uni(yi = c(log(hom.CE.RR), s.1.RR, s.2.RR, s.3.RR, s.4.RR),
                          vi = c(hom.CE.RR.var, s.1.RR.var, s.2.RR.var, s.3.RR.var,
                                 s.4.RR.var))
exp(RR.Hom.ce.comb$b)
c(exp(RR.Hom.ce.comb$ci.lb),exp(RR.Hom.ce.comb$ci.ub))
RR.Hom.ce.comb$tau2

# RR (Random Effects)
RR.Hom.ce.comb <- rma.uni(yi = c(log(hom.CE.RR), s.1.RR, s.2.RR, s.3.RR, s.4.RR),
                          vi = c(hom.CE.RR.var, s.1.RR.var, s.2.RR.var, s.3.RR.var,
                                 s.4.RR.var))
exp(RR.Hom.ce.comb$b)
c(exp(RR.Hom.ce.comb$ci.lb),exp(RR.Hom.ce.comb$ci.ub))
RR.Hom.ce.comb$tau2

# RR (Sep. Entries)
RR.Hom.se.comb <- rma.uni(yi = c(log(RR.hom.1), log(RR.hom.2),
                                 s.1.RR, s.2.RR, s.3.RR, s.4.RR),
                          vi = c(RR.hom.1.var, RR.hom.2.var,
                                 s.1.RR.var, s.2.RR.var, s.3.RR.var,
                                 s.4.RR.var))
exp(RR.Hom.se.comb$b)
c(exp(RR.Hom.se.comb$ci.lb),exp(RR.Hom.se.comb$ci.ub))
RR.Hom.se.comb$tau2

#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

#RD Studies 
tau <- 0.3

s.1.RD <- -0.924
s.2.RD <- 0.199
s.3.RD <- -0.85
s.4.RD <-0.393

s.1.RD.var <- 0.0001
s.2.RD.var <- 0.0016
s.3.RD.var <- 0.0005
s.4.RD.var <- 0.0029

#Collapsed RD

RD.Hom.collapse.comb <-rma.uni(yi = c(RD.hom.c, s.1.RD, s.2.RD, s.3.RD, s.4.RD),
                               vi = c(RD.hom.c.var, s.1.RD.var, s.2.RD.var, 
                                      s.3.RD.var, s.4.RD.var))
RD.Hom.collapse.comb$b
c(RD.Hom.collapse.comb$ci.lb,RD.Hom.collapse.comb$ci.ub)
RD.Hom.collapse.comb$tau2


# RD (MH)
RD.Hom.MH.comb <- rma.uni(yi = c(-0.5, s.1.RD, s.2.RD, s.3.RD, s.4.RD),
                          vi = c(RD.hom.MH.var, s.1.RD.var, s.2.RD.var, s.3.RD.var,
                                 s.4.RD.var))
RD.Hom.MH.comb$b
c(RD.Hom.MH.comb$ci.lb,RD.Hom.MH.comb$ci.ub)
RD.Hom.MH.comb$tau2

# RD (Common Effects)
RD.Hom.ce.comb <- rma.uni(yi = c(hom.CE.RD, s.1.RD, s.2.RD, s.3.RD, s.4.RD),
                          vi = c(hom.CE.RD.var, s.1.RD.var, s.2.RD.var, s.3.RD.var,
                                 s.4.RD.var))
RD.Hom.ce.comb$b
c(RD.Hom.ce.comb$ci.lb,RD.Hom.ce.comb$ci.ub)
RD.Hom.ce.comb$tau2

# RD (Random Effects)
RD.Hom.ce.comb <- rma.uni(yi = c(hom.CE.RD, s.1.RD, s.2.RD, s.3.RD, s.4.RD),
                          vi = c(hom.CE.RD.var, s.1.RD.var, s.2.RD.var, s.3.RD.var,
                                 s.4.RD.var))
RD.Hom.ce.comb$b
c(RD.Hom.ce.comb$ci.lb, RD.Hom.ce.comb$ci.ub)
RD.Hom.ce.comb$tau2

# RD (Sep. Entries)
RD.Hom.se.comb <- rma.uni(yi = c(RD.hom.1, RD.hom.2,
                                 s.1.RD, s.2.RD, s.3.RD, s.4.RD),
                          vi = c(RD.hom.1.var, RD.hom.2.var,
                                 s.1.RD.var, s.2.RD.var, s.3.RD.var,
                                 s.4.RD.var))
RD.Hom.se.comb$b
c(RD.Hom.se.comb$ci.lb,RD.Hom.se.comb$ci.ub)
RD.Hom.se.comb$tau2
