## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)

mkdir("data")

# Load packages

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





# Build FLStock

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








rdata_path <- file.path(getwd(), "rdata")

if (!dir.exists(rdata_path)) {
  dir.create(rdata_path)
}

save(stk,  sr, out, file = file.path(rdata_path, paste0(run, ".rdata")))



ss3rep = SS_output(dir01)
idxs =ss3om::readFLIBss3(dir01) # FLIndexBiomass


## Retune Stock-Recruitment

run = "ane.27.9aS"
load(file=file.path("rdata",paste0(run,".rdata")),verbose = T)
stka = stk

# Extract SR pars
s = params(sr)[1, 2, drop=TRUE]
R0 = params(sr)[2, 2, drop=TRUE]
B0 = params(sr)[3, 2, drop=TRUE]

# single sex
sr1 = srrTMB(as.FLSR(stk,
                     model=bevholtSV),
             spr0=mean(spr0y(stk)),
             r0.pr=c(R0,0.0001),
             s=s,
             s.est=F)

