## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)

mkdir("data")

# Load packages

library(FLBRP)
library(FLasher)
library(FLSRTMB)
library(ss3om)
library(ss3diags)
library(ggpubr)
library(ggplot2)
library(FLCore)
library(ggplotFL)
library(mse)
library(FLRef)
library(ggpubr)
library(mseviz)
library(r4ss)

# Build FLStock

# SS3 outputs are loaded with the `readFLSss3()` into an `FLStock` object. 
# The folder that contains the model outputs has to be specified.
# 
# In the following, the area outside is evaluated first.

dir01 <- "boot/data/Lastyear"
run = "control.SS"
stk = window(ss3om::readFLSss3(dir01))
stk = simplify(stk)
# Fill NAs
stk@m.spwn[] = 0
stk@harvest.spwn[] = 0 
sr = ss3om::readFLSRss3(dir01,run) #A function to read the stock-recruit relationships from an SS3 run into an FLSR object
stk@name = run
stk@desc = "2024, ICES, SS3"
out = ss3om::readOutputss3(dir01)


rdata_path <- file.path(getwd(), "rdata")

if (!dir.exists(rdata_path)) {
  dir.create(rdata_path)
}

save(stk,  sr, out, file = file.path(rdata_path, paste0(run, ".rdata")))



ss3rep = SS_output(dir01)
idxs =ss3om::readFLIBss3(dir01) # FLIndexBiomass


## Retune Stock-Recruitment

run = "control.SS"
load(file=file.path("rdata",paste0(run,".rdata")),verbose = T)
stka = stk

# Extract SR pars
s = params(sr)[1, 2, drop=TRUE]
R0 = params(sr)[2, 2, drop=TRUE]
B0 = params(sr)[3, 2, drop=TRUE]

# single sex
sr1 = srrTMB(as.FLSR(stka,
                     model=bevholtSV),
             spr0=mean(spr0y(stk)),
             r0.pr=c(R0,0.0001),
             s=s,
             s.est=F)

