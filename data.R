# data.R - condition OM(s)
# WKREBUILD_toolset/data.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
mkdir("data")


library(FLSRTMB)
library(FLBRP)

library(FLCore)
library(ggplotFL)
library(FLRef)

library(ss3diags)
library(ggplot2)
library(r4ss)
library(ggpubr)
library(mseviz)

library(FLasher)
library(FLSRTMB)
library(ss3om)

library(mse)
library(dplyr)





# CHOOSE number of cores for doFuture / doParallel
cores <- 3

source("utilities.R")

# LOAD AAP SA results, 2022 ICES WGNSSK sol.27.4
load('boot/data/sol274.rda')

# INTERMEDIATE year
iy <- 2023

# DATA year
dy <- iy - 1


# FINAL year
fy <- 2042

# NUMBER of iterations
it <- 5 #500

# SS3 outputs are loaded with the `readFLSss3()` into an `FLStock` object. 
# The folder that contains the model outputs has to be specified.
# 
# In the following, the area outside is evaluated first.
dir01 <- "boot/data/Lastyear"
stk <- readFLSss3(dir01)
out <- readOutputss3(dir01)

as.data.frame(stk)

rec(stk)[,1]

ssb(stk)
extractSSB(out)
extractFbar(out)

# Cálculo de Fage3 es el promedio de las F de las 4 season/fleets de la edad 3
fatage <- subset(out$fatage,  Era == "TIME")
fatage<-fatage[rowSums(fatage[, c("0", "1", "2", "3")]) != 0, ]
fatage3<-fatage[, c("Yr", "Seas", "3")]


fbar_age3 <- fatage3 %>%
  group_by(Yr) %>%
  summarize(mean_3 = mean(`3`))
# Convertir el dataframe `fbar_age3` en un objeto FLQuant
fbar_age3_FLQ <- FLQuant(fbar_age3$mean_3, dimnames = list(year = fbar_age3$Yr))
dimnames(fbar_age3_FLQ) <- list(
  age = 3,  # Cambiar a "all" si es necesario
  year = fbar_age3$Yr, 
  unit = "unique", 
  season = "all", 
  area = "unique", 
  iter = "1"
)


fqs <- FLQuants(Rec = rec(stk)[, , , season = 3], 
                SSB = ssb(stk)[, , , season = 2], 
                Catch = apply(catch(stk), c(1, 2), sum),
                F =  fbar_age3_FLQ)

plot(fqs)



ggplot(data=fqs, aes(year, data)) + 
  geom_line() + facet_wrap(~qname, scales="free_y", nrow=4) + labs(x="", y="")

# Plot the SSB-Recruits graph
fqs_df<-as.data.frame(fqs)
fqs_df <- subset(fqs_df, qname %in% c("SSB", "Rec"))

# Separar en columnas de SSB y Rec
fqs_wide <- reshape(fqs_df, idvar = "year", timevar = "qname", direction = "wide")

# Renombrar columnas para claridad
colnames(fqs_wide) <- c("Year", "SSB", "Rec")

# Graficar la relación SSB vs Reclutamiento
ggplot(fqs_wide, aes(x = data.SSB, y = data.Rec)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "SSB vs Recruits", x = "SSB", y = "Recruits") +
  theme_minimal()

sr1 <- FLSR()
as.FLSR(fqs)

# Fill NAs
stk@m.spwn[] = 0
stk@harvest.spwn[] = 0 
sr = ss3om::readFLSRss3(dir01,run) #A function to read the stock-recruit relationships from an SS3 run into an FLSR object

stk@name = run
stk@desc = "2024, ICES, SS3"


set.seed(987)

#'*===========================================================*
# - Stock-recruitment relationship(s) ----
#'*===========================================================*
#'
# FIT models
fits <- srrTMB(as.FLSR(run, 
                       model=bevholtSV), 
               spr0=mean(spr0y(run)))


# PLOT
plotsrs(fits)


# BOOTSTRAP and SELECT model by largest logLik **
srpars <- bootstrapSR(run, 
                      iters=it,
                      model=bevholtSV,
                      method="best")

# SAVE
save(fits, srpars, file="data/bootstrap.rda", compress="xz")


#'*===========================================================*
# - CONSTRUCT OM ----
#'*===========================================================*

# GENERATE future deviances: lognormal autocorrelated **
srdevs <- rlnormar1(sdlog=srpars$sigmaR, 
                    rho=srpars$rho,
                    years=seq(dy, fy))

plot(srdevs)

# BUILD FLom
om <- FLom(stock=propagate(run, it), 
           refpts=refpts, 
           model='mixedsrr',
           params=srpars, 
           deviances=srdevs)

# SETUP om future: average of last 3 years **
om <- fwdWindow(om, end=fy)

# SET stochastic rec dy
rec(stock(om))[, '2022'] <- rec(om)[1, '2022'] * srdevs[, '2022']

# PROJECT forward for iy assumption
om <- fwd(om, catch=FLQuant(4289.2, dimnames=list(year=2023)))

# F and SSB deviances
sdevs <- shortcut_devs(om, Fcv=0.212, Fphi=0.423, SSBcv=0.10)


# - CONSTRUCT iem, implementation error module w/10% noise **

iem <- FLiem(method=noise.iem,
             args=list(noise=rlnorm(500, rec(om) %=% 0, 0.1)))

<<<<<<< HEAD
#'*===========================================================*
# - SAVE ----
#'*===========================================================*
save(om, iem, sdevs, file="data/data.rda", compress="xz")
=======
# single sex
sr1 = srrTMB(as.FLSR(stk,
                     model=bevholtSV),
             spr0=mean(spr0y(stk)),
             r0.pr=c(R0,0.0001),
             s=s,
             s.est=F)
>>>>>>> a68d777a06e79dd688dbc3f165c67b7dbfc0738b

plan(sequential)