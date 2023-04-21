
library(metafor)
# OR data 

s.4.RR.var

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

OR.het.collapse.comb <-rma.uni(yi = c(log(OR.het.c), s.1.OR, s.2.OR, s.3.OR, s.4.OR),
                               vi = c(OR.het.c.var, s.1.OR.var, s.2.OR.var, s.3.OR.var, s.4.OR.var))
exp(OR.het.collapse.comb$b)
c(exp(OR.het.collapse.comb$ci.lb),exp(OR.het.collapse.comb$ci.ub))
OR.het.collapse.comb$tau2


# OR (MH)
OR.het.MH.comb <- rma.uni(yi = c(log(OR.het.MH), s.1.OR, s.2.OR, s.3.OR, s.4.OR),
                          vi = c(OR.het.MH.var, s.1.OR.var, s.2.OR.var, s.3.OR.var,
                                 s.4.OR.var))
exp(OR.het.MH.comb$b)
c(exp(OR.het.MH.comb$ci.lb),exp(OR.het.MH.comb$ci.ub))
OR.het.MH.comb$tau2

# OR (Peto)
OR.het.peto.comb <- rma.uni(yi = c(log(het.peto), s.1.OR, s.2.OR, s.3.OR, s.4.OR),
                            vi = c(OR.het.peto.var, s.1.OR.var, s.2.OR.var, s.3.OR.var,
                                   s.4.OR.var))
exp(OR.het.peto.comb$b)
c(exp(OR.het.peto.comb$ci.lb),exp(OR.het.peto.comb$ci.ub))
OR.het.peto.comb$tau2


# OR (Common Effects)
OR.het.ce.comb <- rma.uni(yi = c(log(het.CE.OR), s.1.OR, s.2.OR, s.3.OR, s.4.OR),
                          vi = c(het.CE.OR.var, s.1.OR.var, s.2.OR.var, s.3.OR.var,
                                 s.4.OR.var))
exp(OR.het.ce.comb$b)
c(exp(OR.het.ce.comb$ci.lb),exp(OR.het.ce.comb$ci.ub))
OR.het.ce.comb$tau2

# OR (Random Effects)
OR.het.re.comb <- rma.uni(yi = c(log(het.RE.OR), s.1.OR, s.2.OR, s.3.OR, s.4.OR),
                          vi = c(het.RE.OR.var, s.1.OR.var, s.2.OR.var, s.3.OR.var,
                                 s.4.OR.var))
exp(OR.het.re.comb$b)
c(exp(OR.het.re.comb$ci.lb),exp(OR.het.re.comb$ci.ub))
OR.het.re.comb$tau2

# OR (Sep. Entries)
OR.het.se.comb <- rma.uni(yi = c(log(OR.het.1), log(OR.het.2),
                                 s.1.OR, s.2.OR, s.3.OR, s.4.OR),
                          vi = c(OR.het.1.var, OR.het.2.var,
                                 s.1.OR.var, s.2.OR.var, s.3.OR.var,
                                 s.4.OR.var))
exp(OR.het.se.comb$b)
c(exp(OR.het.se.comb$ci.lb),exp(OR.het.se.comb$ci.ub))
OR.het.se.comb$tau2


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

RR.het.collapse.comb <-rma.uni(yi = c(log(RR.het.c), s.1.RR, s.2.RR, s.3.RR, s.4.RR),
                               vi = c(RR.het.c.var, s.1.RR.var, s.2.RR.var, s.3.RR.var, s.4.RR.var))
exp(RR.het.collapse.comb$b)
c(exp(RR.het.collapse.comb$ci.lb),exp(RR.het.collapse.comb$ci.ub))
RR.het.collapse.comb$tau2


# RR (MH)
RR.het.MH.comb <- rma.uni(yi = c(log(1.03), s.1.RR, s.2.RR, s.3.RR, s.4.RR),
                          vi = c(RR.het.MH.var, s.1.RR.var, s.2.RR.var, s.3.RR.var,
                                 s.4.RR.var))
exp(RR.het.MH.comb$b)
c(exp(RR.het.MH.comb$ci.lb),exp(RR.het.MH.comb$ci.ub))
RR.het.MH.comb$tau2


# RR (Common Effects)
RR.het.ce.comb <- rma.uni(yi = c(log(het.CE.RR), s.1.RR, s.2.RR, s.3.RR, s.4.RR),
                          vi = c(het.CE.RR.var, s.1.RR.var, s.2.RR.var, s.3.RR.var,
                                 s.4.RR.var))
exp(RR.het.ce.comb$b)
c(exp(RR.het.ce.comb$ci.lb),exp(RR.het.ce.comb$ci.ub))
RR.het.ce.comb$tau2

# RR (Random Effects)
RR.het.re.comb <- rma.uni(yi = c(log(het.RE.RR), s.1.RR, s.2.RR, s.3.RR, s.4.RR),
                          vi = c(het.RE.RR.var, s.1.RR.var, s.2.RR.var, s.3.RR.var,
                                 s.4.RR.var))
exp(RR.het.re.comb$b)
c(exp(RR.het.re.comb$ci.lb),exp(RR.het.re.comb$ci.ub))
RR.het.re.comb$tau2

# RR (Sep. Entries)
RR.het.se.comb <- rma.uni(yi = c(log(RR.het.1), log(RR.het.2),
                                 s.1.RR, s.2.RR, s.3.RR, s.4.RR),
                          vi = c(RR.het.1.var, RR.het.2.var,
                                 s.1.RR.var, s.2.RR.var, s.3.RR.var,
                                 s.4.RR.var))
exp(RR.het.se.comb$b)
c(exp(RR.het.se.comb$ci.lb),exp(RR.het.se.comb$ci.ub))
RR.het.se.comb$tau2

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

RD.het.collapse.comb <-rma.uni(yi = c(RD.het.c, s.1.RD, s.2.RD, s.3.RD, s.4.RD),
                               vi = c(RD.het.c.var, s.1.RD.var, s.2.RD.var, 
                                      s.3.RD.var, s.4.RD.var))
RD.het.collapse.comb$b
c(RD.het.collapse.comb$ci.lb,RD.het.collapse.comb$ci.ub)
RD.het.collapse.comb$tau2


# RD (MH)
RD.het.MH.comb <- rma.uni(yi = c(-0.5, s.1.RD, s.2.RD, s.3.RD, s.4.RD),
                          vi = c(RD.het.MH.var, s.1.RD.var, s.2.RD.var, s.3.RD.var,
                                 s.4.RD.var))
RD.het.MH.comb$b
c(RD.het.MH.comb$ci.lb,RD.het.MH.comb$ci.ub)
RD.het.MH.comb$tau2


# RD (Common Effects)
RD.het.ce.comb <- rma.uni(yi = c(het.CE.RD, s.1.RD, s.2.RD, s.3.RD, s.4.RD),
                          vi = c(het.CE.RD.var, s.1.RD.var, s.2.RD.var, s.3.RD.var,
                                 s.4.RD.var))
RD.het.ce.comb$b
c(RD.het.ce.comb$ci.lb,RD.het.ce.comb$ci.ub)
RD.het.ce.comb$tau2

# RD (Random Effects)
RD.het.re.comb <- rma.uni(yi = c(het.RE.RD, s.1.RD, s.2.RD, s.3.RD, s.4.RD),
                          vi = c(het.RE.RD.var, s.1.RD.var, s.2.RD.var, s.3.RD.var,
                                 s.4.RD.var))
RD.het.re.comb$b
c(RD.het.re.comb$ci.lb, RD.het.re.comb$ci.ub)
RD.het.re.comb$tau2

# RD (Sep. Entries)
RD.het.se.comb <- rma.uni(yi = c(RD.het.1, RD.het.2,
                                 s.1.RD, s.2.RD, s.3.RD, s.4.RD),
                          vi = c(RD.het.1.var, RD.het.2.var,
                                 s.1.RD.var, s.2.RD.var, s.3.RD.var,
                                 s.4.RD.var))
RD.het.se.comb$b
c(RD.het.se.comb$ci.lb,RD.het.se.comb$ci.ub)
RD.het.se.comb$tau2
