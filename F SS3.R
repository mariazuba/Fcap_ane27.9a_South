# F-based forecasts with SS3 and FLR ----

library(r4ss)
library(FLRef)
library(ss3om)
library(ss3diags)

# 1.1 Step 1: Set up file paths and folders structure for loading and saving the SS3 model ----
# Set up the file path to the folder where the SS3 model folder with run is located.
# Define name of reference model folder with the SS3 model outputs

model = "boot/data/Lastyear"

# 2.1 Step 1: Basic setup
# 
# In this a case, a folder with the reference model run is created and the model outputs are loaded with
# r4ss::SS_output
# next set up the folder where the SS3 model folder with run is located
# Load the assessment model
ss3rep = readRDS(file.path("rdata", stock.file))

# To organise the forecast outputs, first create a subfolder forecast
forecast.dir = file.path(model, "forecast")
dir.create(forecast.dir, showWarnings = F)

# A new helper function SSnewrun was added to ss3diags to easily create subfolders for the forecast scenarios.
# First a Fmsy reference folder is created
# To this specify a new subfolder path, where to run the forecast for a “fixed” FM SY scenarios
fmsydir = file.path(forecast.dir, "Fmsy")

# Create new F forecast model folder. Note that the data and control file and ss.exe names need to be specified
# if these diverge from the defaults data.ss, control.ss and ss3.exe
dat = "data.SS"
ctl = "control.SS"
ss.exe = "ss3"
SSnewrun(model = file.path(model), dat = dat, ctl = ctl, newdir = fmsydir,
         ss.exe = "ss3")

# Now the forecast file can be read be read with r4ss
fc <- SS_readforecast(file.path(fmsydir, "forecast.ss"), verbose = F)

#2.2 Step 2: Initial F exploitation calculations for Fapic forecast
#Extract the $exploitation output from the report file
Fexp = ss3rep$exploitation

# Importantly, the annual_F are scaled to the F-basis (here Fbar ), whereas fleet specific F values are always
# given as Fapic
# Next compute the combined Fapic generically across fleets
Fexp$Fapic = apply(as.matrix(ss3rep$exploitation[,-c(1:6)]),
                   1, sum, na.rm = T)

# and aggregate across seasons, by taking the mean and not the sum.
Fapic = aggregate(Fapic~ Yr, Fexp, mean)
# Next compute the corresponding annual Fbar values from the annual_F
Fbar = aggregate(annual_F~ Yr, Fexp, mean)

# To work out exact ratio between Fapic and Fbar so that it is consistent with the benchmark calculations with
# ss3, it is necessary to extract the reference years for selectivity from the forecast.ss file.

# The information required for the average selectivity conditions can be found in the forecast.ss file under
# $Bmark_years. The third and fourth position define the time horizon for the average selectivity across fleet,
# a value of -999 (here) indicates that the whole time series is use, but more commonly averages are taken,
# e.g. over the last 3 years, which can be specified as -2 0 or 2020 2022. The following code attempts to
# compute this generically.

endyr = ss3rep$endyr
if (fc$Bmark_years[3] < -90) {
  nfc = length(min(ss3rep$exploitation$Yr + 1):endyr) # excluded init year
} else {
  # if specified (e.g. -2, 0)
  nfc = fc$Bmark_years[4]- fc$Bmark_years[3] + 1
}
# Benchmark reference years
bmyrs = (endyr- nfc + 1):endyr
bmyrs

# NOTE: Other than here, it recommended to set the Bmark_years in the forecast.ss so that all quantities
# represent the mean last 3 years (i.e. -2). The advantage is that this allows using be consistent with the
# default FLR settings.

Fratio = mean(Fapic$Fapic[Fapic$Yr %in% max(bmyrs)]/Fbar$annual_F[Fbar$Yr %in% max(bmyrs)])
Fratio

#'*REVISAR ESTA SECCIÓN, PORQUE NO TENGO FMSY EN ANE.27.9AS!!!!!*
# Fratio defines the ratio of Fapic to Fbar for the reference period !!
# Set FM SY to benchmark
Fmsy = bms["Fmsy"][[1]]
# This value is given as Fbar and therefore needs to be transformed to Fapic
# Fmsy.apic = Fmsy * Fratio
# Fmsy # Fbar
# 
# Fmsy.apic

# 2.3 Step 3: Setting up the manual F forecast input structure
# First, do some basic house keeping for the model structure. This is designed to work generically for any
# multi-fleet or seasonal structure

nseas = length(unique(ss3rep$exploitation$Seas)) # number of seasons
fleets = unique(ss3rep$fatage$Fleet) # fleets
nfleets = length(fleets) # number of fleet

# Next, the mean Fapic by fleet and season is calculated

# subset to benchmark years for selectivity
fexp = ss3rep$exploitation[ss3rep$exploitation$Yr %in% bmyrs,]
fexp = cbind(fexp[, 1:2], fexp[,-c(1:5)])[,-3] #><> single fleet trick
# flip
fexp = reshape2::melt(fexp, id.vars = c("Yr", "Seas"), variable.name = "Fleet",
                      value.name = "Fapic")
tail(fexp)

# The forecast file requires Fleet IDs not names. In the next step these are extracted and fleet names are
# converted in to Fleet IDs
fleet = data.frame(Fleet = ss3rep$FleetNames, ID = ss3rep$fleet_ID)
fexp$Fleet = fleet[match(fexp$Fleet, fleet$Fleet), 2]

# Then, the relative proportions of Fapic by fleet and season can be computed
Fap = aggregate(Fapic~ Seas + Fleet, fexp, mean)
Fap$prop = Fap$Fapic/sum(Fap$Fapic) * nseas
Fap

# In the next step, status quo Fsq for forecasting over the intermediate year(s) is defined. This can be relatively
# easily changed to intermediate catch years. Here, the Fsq is taken as F2022 to account for the systematically
# decreasing trend, and the intermediate years are set to 1, account for 1 data lag year.
# F status q
nfsq = 1 #'*Lo debo dejar en 0 porque no tenemos año intermedio- REVISAR!!!* 
nint = 1 #'*Lo debo dejar en 0 porque no tenemos año intermedio- REVISAR!!!*
# Compute the Fsq as Fapic vector by season and fleet
fsq = ss3rep$exploitation[ss3rep$exploitation$Yr %in% ((endyr-nfsq + 1):endyr), ]
fsq = cbind(fsq[, 1:2], fsq[,-c(1:5)])[,-3] #><> single fleet trick
fsq = reshape2::melt(fsq, id.vars = c("Yr", "Seas"), variable.name = "Fleet",
                     value.name = "Fapic")
Fsq = aggregate(Fapic~ Seas + Fleet, fsq, mean)

# Now, the forecast horizon can be defined in the loaded starter.ss object fc. Summary statistics on catch
# opportunities require a three year forecast horizon:
# (1) Intermediate year based on Fsq or TAC
# (2) Implementation year with catch and F outcomes
# (3) One-step-ahead forecast of the SSB response and recruitment, when spawning is set to 1st of January
# (default)

fc$Nforecastyrs = 3
nfyrs = fc$Nforecastyrs
fyrs = endyr + c(1:nfyrs)

# The F-vector that is passed on the forecast file comprises the season/fleet structure replicates for ninit for
# Fsq and the forecast years under Ftgt that is scaled to Fapic by the Fratio and portioned by fleets.
fvec = c(rep(Fsq$Fapic, nint), rep(Fmsy * Fratio * Fap$prop, nfyrs- nint))

# Giventhefleet,season,intermediateyearandforecastyearsstructures,theforecasttablefortheforecast.ss
# file can finally be constructed.
fc$ForeCatch = data.frame(Year = rep(fyrs, each = nseas * nfleets),
                          Seas = 1:nseas, Fleet = rep(fleets, each = nseas), catch_or_F = fvec,
                          Basis = 99)
tail(fc$ForeCatch, 9)




