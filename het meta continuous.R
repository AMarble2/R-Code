
library(metafor)
#Combining the hetogeneous data with 4 other studies 

#Mean Difference 

#Study data
s.1.MD <- -1.44
s.2.MD <- -1.35
s.3.MD <- -2.42
s.4.MD <- 1.48

s.1.MD.var <- 0.48
s.2.MD.var <- 0.49
s.3.MD.var <- 0.40
s.4.MD.var <- 0.23


#Mean Difference (Collapsed)

rma(yi = c(het.c.MD, s.1.MD, s.2.MD, s.3.MD, s.4.MD),
    vi= c(het.c.MD.var, s.1.MD.var, s.2.MD.var, s.3.MD.var, s.4.MD.var))

#Mean Difference (CE)

rma(yi = c(het.ce.MD, s.1.MD, s.2.MD, s.3.MD, s.4.MD),
    vi= c(het.ce.MD.var, s.1.MD.var, s.2.MD.var, s.3.MD.var, s.4.MD.var))

#Mean Difference (RE)
rma(yi = c(het.re.MD, s.1.MD, s.2.MD, s.3.MD, s.4.MD),
    vi= c(het.re.MD.var, s.1.MD.var, s.2.MD.var, s.3.MD.var, s.4.MD.var))

#Mean Difference (Separate Entries)
rma(yi = c(het.1.MD,het.2.MD, s.1.MD, s.2.MD, s.3.MD, s.4.MD),
    vi= c(het.1.MD.var,het.2.MD.var, s.1.MD.var, 
          s.2.MD.var, s.3.MD.var, s.4.MD.var))

####################################################################################
####################################################################################
####################################################################################
####################################################################################

#Cohen's D 

#Studies 

s.1.CD <- -0.343
s.2.CD <- 0.159
s.3.CD <- -0.243
s.4.CD <- -0.641

s.1.CD.var <- 0.109
s.2.CD.var <- 0.016
s.3.CD.var <- 0.074
s.4.CD.var <- 0.156

#Cohen's D collapsed 

#Cohen's D (Collapsed)

rma(yi = c(het.c.Cohen.D, s.1.MD, s.2.MD, s.3.MD, s.4.MD),
    vi= c(het.c.Cohen.D.var, s.1.MD.var, s.2.MD.var, s.3.MD.var, s.4.MD.var))

#Cohen's D (CE)

rma(yi = c(het.CE.Cohen.D, s.1.MD, s.2.MD, s.3.MD, s.4.MD),
    vi= c(het.CE.Cohen.D.var, s.1.MD.var, s.2.MD.var, s.3.MD.var, s.4.MD.var))

#Cohen's D (RE)
rma(yi = c(het.re.CD, s.1.MD, s.2.MD, s.3.MD, s.4.MD),
    vi= c(het.re.CD.var, s.1.MD.var, s.2.MD.var, s.3.MD.var, s.4.MD.var))

#Cohen's D (Separate Entries)
rma(yi = c(het.1.Cohen.D,het.2.Cohen.D, s.1.MD, s.2.MD, s.3.MD, s.4.MD),
    vi= c(het.1.Cohen.D.var,het.2.Cohen.D.var, s.1.MD.var, 
          s.2.MD.var, s.3.MD.var, s.4.MD.var))


####################################################################################
####################################################################################
####################################################################################
####################################################################################

#Hedges' g 

#Studies 

s.1.hg <- 0.157
s.2.hg <- -0.345
s.3.hg <- -0.641
s.4.hg <- -0.172

s.1.hg.var <- 0.020
s.2.hg.var <- 0.036
s.3.hg.var <- 0.070
s.4.hg.var <- 0.009

#Hedges' g (Collapsed)

rma(yi = c(het.c.hedge.g, s.1.MD, s.2.MD, s.3.MD, s.4.MD),
    vi= c(het.c.hedge.g.var, s.1.MD.var, s.2.MD.var, s.3.MD.var, s.4.MD.var))

#Hedges' g (CE)

rma(yi = c(het.CE.hedge.g, s.1.MD, s.2.MD, s.3.MD, s.4.MD),
    vi= c(het.CE.hedge.g.var, s.1.MD.var, s.2.MD.var, s.3.MD.var, s.4.MD.var))

#Hedges' g (RE)
rma(yi = c(het.re.hg, s.1.MD, s.2.MD, s.3.MD, s.4.MD),
    vi= c(het.re.hg.var, s.1.MD.var, s.2.MD.var, s.3.MD.var, s.4.MD.var))

#Cohen's D (Separate Entries)
rma(yi = c(het.1.hedge.g,het.2.hedge.g, s.1.MD, s.2.MD, s.3.MD, s.4.MD),
    vi= c(het.1.hedge.g.var,het.2.hedge.g.var, s.1.MD.var, 
          s.2.MD.var, s.3.MD.var, s.4.MD.var))
