# RHEM
## An R package for generating input files for the Rangeland Hydrology and Erosion Model (RHEM)

This package seeks to replicate the text inputs generated by the [RHEM webtool](https://apps.tucson.ars.ag.gov/rhem/tool). 
Additionally a few (clunky) functions are included for running RHEM models locally with an 
[executable file](https://apps.tucson.ars.ag.gov/rhem/docs)

## Download

```
devtools::install_github('fickse/RHEM')
```

## Usage:


### Generate a .par file

```
library(RHEM)

# you can see the list of parameter names and default settings with par_defaults()
par_defaults()

# add parameters as named arguments
p <- build_par( scenarioname = 'test', OUTPUT_FOLDER = tempdir(), soiltexture = 'clay loam', slopesteepness = 20 )
p

file.exists(p$soilFileName)
```

### Generate storm file (.pre)

```
# create a cli-gen like storm file 
# see /example/cligen.R for an example of how to generate storm data from climate station data (from mesowest)

climatedata <- structure(list(id = 1:20, day = c(1L, 19L, 20L, 21L, 22L, 23L, 
  27L, 28L, 30L, 6L, 7L, 16L, 19L, 20L, 21L, 25L, 26L, 4L, 5L, 
  7L), month = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 
  2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L), year = c(2010L, 2010L, 2010L, 
  2010L, 2010L, 2010L, 2010L, 2010L, 2010L, 2010L, 2010L, 2010L, 
  2010L, 2010L, 2010L, 2010L, 2010L, 2010L, 2010L, 2010L), rain = c(19.304, 
  4.572, 2.032, 1.016, 7.874, 1.27, 2.286, 9.652, 0.254, 5.842, 
  3.556, 2.032, 1.016, 8.636, 1.27, 1.27, 0.254, 0.762, 2.032, 
  5.08), duration = c(3, 2, 2, 2, 8, 1, 6, 2, 1, 1, 8, 1, 5, 3, 
  1, 1, 1, 2, 3, 2), Tp = c(0.166666666666667, 0.25, 0.25, 0.75, 
  0.9375, 0.5, 0.0833333333333333, 0.25, 0.5, 0.5, 0.0625, 0.5, 
  0.1, 0.5, 0.5, 0.5, 0.5, 0.25, 0.5, 0.75), Ip = c(12.954, 3.81, 
  1.778, 0.762, 3.81, 1.27, 0.762, 5.588, 0.254, 5.842, 1.016, 
  2.032, 0.762, 5.842, 1.27, 1.27, 0.254, 0.508, 1.524, 3.556)), .Names = c("id", 
  "day", "month", "year", "rain", "duration", "Tp", "Ip"), row.names = c(NA, 
  20L), class = "data.frame")

stormf <- file.path(tempdir, 'storm.pre')
createStormFile( climatedata, stormf , TITLE = 'TEST STORM DATA', CREATION_DATE = Sys.time() )

```

If you know the path to your local executable, you can run RHEM and retrieve results in a data.frame 

```
parfile <- p$soilFileName
prefile <- stormf
exe <- 'path_to_executable'
outfile <- 'run_1.sum'

r <- run_rhem( parfile, prefile, exe, outfile)

r
```
