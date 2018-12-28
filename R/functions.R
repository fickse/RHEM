#' Read output
#'
#' @param file path to .out file.
#' @details creates a temporary '.run' file and executes it
read.rhem <- function(f) {

	r <- readLines(f)
	if(length(r) < 3) stop( 'no data in file ', f )
	r <- sapply(r, function(x) gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl=TRUE))
	rr <- lapply(r, function(x) strsplit(x, ' .*?'))
	x <- lapply(rr, function(x) x[[1]])
	y <- do.call(rbind, x)
	namez <- paste0(y[1,], '_', y[2,])
	y <- y[-c(1,2),]
	y <- apply(y, 2, as.numeric)
	y <- as.data.frame(y)
	names(y) <- namez
	y
}




#' Format climate database
#'
#' @param data A data.frame.
#' Run Rhem
#'
#' @param parlist path.
#' @param prefile path.
#' @param output path.
#' @param executable path to executable.
#' @details creates a temporary '.run' file and executes it
run_rhem <- function( parlist, prefile, exe, cleanup = TRUE){

	require(raster)

	#add .sum suffix
#	if(!grepl('[.]sum$', outfile)) outfile <- paste0( gsub('[.]*', '', outfile), '.sum')

   #outfile <- file.path(parlist$OUTPUT_FOLDER, paste0(parlist$

    outfile <- gsub('[.]par', '.sum', parlist$soilFileName)

	# create .run file
	runfile <- extension(basename(outfile), 'run')
	#runfile <- file.path(tempdir(), runfile)
	cat(parlist$soilFileName, ', ', prefile, ', ', basename(outfile), ', "Scenario Name",0,2,y,y,n,n,y', # note I can't figure out what all of these parameters are since the documentation is nonexistent.
	 file = runfile, sep = '')

#	td <- tempdir()
#	file.copy(executable, td)
#	file.copy(parlist, td, overwrite=TRUE)
#	file.copy(prefile, td, overwrite = TRUE)

#	od <- getwd()
#	setwd(td)
#	command <- paste0(file.path(td, basename(executable)), ' -b ', runfile)
	command <- paste0(exe, ' -b ', runfile)
	resp <- system(command, intern = T)
#	setwd(od)

#	resultsfile <- file.path(td, extension( basename(outfile), '.out'))
#	file.copy(file.path(td, basename(outfile)), dirname(outfile), overwrite = TRUE)
#	file.copy(resultsfile, dirname(outfile), overwrite = TRUE)

    ff <- list.files(pattern = gsub('[.]sum', '', basename(outfile)))

	r <- read.rhem(grep('out', ff, value = TRUE))

    a <- get_rhem_sums(basename(outfile))


    file.rename(ff, file.path(parlist$OUTPUT_FOLDER, ff))
#	if(cleanup) unlink(td, recursive = TRUE)
	return( list(averages = a, runfile = runfile, outfile = outfile, command = command, resp = resp, out = r))

}

#' Parse .sum file
#'
#' @param sumfile path to .sum output
get_rhem_sums <- function(sumfile){

    v <- readLines(sumfile)
    out <- list(
    avg_runoff_mm_yr = gsub( '.* ', '', grep('Avg-Runoff[(]mm/year[)]=', v, value = TRUE)),
    avg_loss_tn_ha_yr = gsub( '.* ', '', grep('Avg-Soil-Loss[(]ton/ha/year[)]=', v, value = TRUE)),
    avg_yield_tn_ha_yr = gsub( '.* ', '', grep('Avg-SY[(]ton/ha/year[)]=', v, value = TRUE))
    )
    return(out)

}



#' Create Storm File
#'
#' @param outfile target .pre file.
#' @param ... list of key:value pairs that are written to the header of the file
#' @return .pre file
#' @details Input dataframe should have the columns corresponding to the input .pre file, including the following columns:
#' \describe{
#'   \item{day}{day of month}
#'   \item{month}{month}
#'   \item{year}{year}
#'   \item{rain}{daily rainfall}
#'   \item{duration}{duration in hours}
#'   \item{Tp}{fractional time to peak intensity}
#'   \item{Ip}{Rainfall intensity at peak (mm / hr)}
#' }
createStormFile <- function(data, outfile, ...) {
	require(gdata)

	preamble <- list(...)

	txt <- sapply(names(preamble), function(i) paste0("# ", i, " : ", preamble[[i]]))

	nevents <- nrow(data)

	txt <- c(txt, paste0( nevents, ' # The number of rain events'))
	txt <- c(txt, paste0(0, ' # Breakpoint data? (0 for no, 1 for yes'))
	txt <- c(txt, '#  id     day  month  year  Rain   Dur    Tp     Ip')


	first <- rep ('    ', nrow(data))
	id <- sprintf('%-6d', 1:nrow(data))
	day <-sprintf('%-6d', data$day)
	month <-sprintf('%-6d', data$month)
	year <-sprintf('%-6d', data$year)
	rain <-formatC(data$rain, width = -7, digits = 1, format = 'f')
	duration <- formatC(data$duration, width = -7, digits = 2, format = 'f')
	Tp <- formatC(data$Tp, width = -7, digits = 2, format = 'f')
	Ip <- formatC(data$Ip,  digits = 2, format = 'f')

	dd <- data.frame(first,id, day, month, year, rain, duration, Tp, Ip)
	txt2 <- do.call(paste0, dd)

	writeLines(c(txt,txt2), outfile)
}



#' Format Slope parameters
#'
#' @param slopeshape a character, one of 's-shaped', 'convex', 'concave', 'uniform'
#' @param slopesteepness integer percent grade of slope
#' @return formatted character string input for .par file
#' @examples
#' createslopeParameters('s-shaped', 40)
#' createslopeParameters('s-shaped', 80)
#' createslopeParameters('uniform', 40)
#'
createSlopeParameters <- function(slopeshape,slopesteepness){

  # Uniform Slope
  if(slopeshape == 'uniform'){

  SL <- rep(slopesteepness, 2)
  SX <- c(0.00, 1.00)

  } else if ( slopeshape == 'concave') {
  # Concave Slope
  SL <- c(slopesteepness*2,  0.001)
  SX <- c(0.00,1.00)

  } else if ( slopeshape == 'convex'){
  # Convex Slope
  SL <- c(0.001, 2*slopesteepness)
  SX <- c(0,1)
  } else if ( slopeshape == 's-shaped'){
  # S-shaped Slope
  SL <- c(.001, 2*slopesteepness, .001)
  SX <- c(0, .5, 1)
  }

  SLline <- paste0("\t\tSL\t=\t", paste(format(round(SL,3), nsmall = 3), collapse = '\t,\t'))
  SXline <- paste0("\t\tSX\t=\t", paste(format(round(SX,3), nsmall = 3), collapse = '\t,\t'))

  return( paste0(SLline, '\n', SXline, '\n'))
 }





#' Write .par file for input to rhem
#'
#' @param ... named arguments. see \code{details}
#' @details the complete list of input parameters may be found via the \code{\link{par_defaults}} function. Parameters include:
#' \describe{
#'   \item{scenarioname}{ (defaults to \code{as.numeric(Sys.time())})}
#'   \item{units}{ 'Metric' or 'English' }
#'   \item{soiltexture}{ see \code{\link{texture_df}} }
#'   \item{moisturecontent}{ Initial moisture content \% saturation ( default = 25)}
#'   \item{bunchgrasscanopycover}{ integer (\%)}
#'   \item{forbscanopycover}{ integer (\%)}
#'   \item{shrubscanopycover}{ integer (\%)}
#'   \item{sodgrasscanopycover}{ integer (\%)}
#'   \item{rockcover}{ integer (\%)}
#'   \item{basalcover}{ integer (\%)}
#'   \item{littercover}{ integer (\%)}
#'   \item{cryptogamscover}{ integer (\%)}
#'   \item{slopelength }{ integer (m)}
#'   \item{slopeshape }{ "uniform", "convex", "concave" or "s-shaped"}
#'   \item{slopesteepness }{ integer (\%)}
#'   \item{version }{ character (currently no effect)}
#'   \item{OUTPUT_FOLDER}{place to save .par file. Defaults to '.'}
#'   \item{prefix}{optional prefix for output files}
#' }
#' @return list of inputs for .par file
#' @examples
#' a <- build_par() # defaults
#' a
#' unlink(a$handle)
#'
build_par <- function(mylist){

  # y <- list(...)
  y <- mylist

  x <- par_defaults()

  for( n in names(y)){

    if (! n %in% names(x)) warning('variable name ', n, ' not recognized. See parameter defaults with par_defaults')

    x[[n]] <- y[[n]]

  }

  if(is.null(x$prefix)){ x$prefix <- "scenario_input_" }

  # quality control checks here...

        #coerce to numeric
        nums <- c('slopelength', 'slopesteepness','bunchtrasscanopycover','forbscanopycover','shrubscanopycover','sodgrasscanopycover','totalcanopycover','rockcover', 'basalcover','littercover','cryptogamscover')

        for(n in nums){

            x[[n]] <- as.numeric(x[[n]])

        }

        x$slopeshape <- tolower(x$slopeshape)
        x$soiltexture <- tolower(x$soiltexture)

		# convert to percent values
		x$bunchgrasscanopycover = x$bunchgrasscanopycover/100
		x$forbscanopycover = x$forbscanopycover/100
		x$shrubscanopycover = x$shrubscanopycover/100
		x$sodgrasscanopycover = x$sodgrasscanopycover/100

		# canopy cover for grass (this is for the new Kss equations from Sam)
		x$grasscanopycover = x$bunchgrasscanopycover + x$forbscanopycover + x$sodgrasscanopycover

		x$moisturecontent = x$moisturecontent/100

		#
		# TOTAL CANOPY COVER
		x$totalcanopycover = x$bunchgrasscanopycover + x$forbscanopycover + x$shrubscanopycover + x$sodgrasscanopycover

		x$rockcover = x$rockcover/100
		x$basalcover = x$basalcover/100
		x$littercover = x$littercover/100
		x$cryptogamscover = x$cryptogamscover/100

		#////
		#// TOTAL GROUND COVER
		x$totalgroundcover = x$basalcover + x$littercover + x$cryptogamscover + x$rockcover

		x$slopesteepness = x$slopesteepness/100

		#// get the soil information from the database

    x$texturerow = get_soil_texture(x$soiltexture)
		#x$meanclay = x$texturerow$mean_clay
		x$meanmatricpotential = x$texturerow$mean_matric_potential
		x$poresizedistribution = x$texturerow$pore_size_distribution_index
		x$meanporosity = x$texturerow$mean_porosity

		#// compute ft (replaces fe and fr)
		x$ft =  ( -1 * 0.109) + (1.425 * x$littercover) + (0.442 * x$rockcover) + (1.764 * (x$basalcover + x$cryptogamscover)) + (2.068 * x$slopesteepness)
		x$ft = 10^x$ft


		#// Implement the new equations to calculate Ke.

    ke_parms <- list(

		"sand" = c(24,0.3483),
		"loamy sand"=c(10 ,0.8755 ),
		"sandy loam"=c(5 ,1.1632 ),
		"loam"=c(2.5 ,1.5686 ),
		"silt loam"=c(1.2 ,2.0149 ),
		"silt"=c(1.2 ,2.0149 ),
		"sandy clay loam"=c(0.80 ,2.1691 ),
		"clay loam"=c(0.50 ,2.3026 ),
		"silty clay loam"=c(0.40 ,2.1691 ),
		"sandy clay"=c(0.30 ,2.1203 ),
		"silty clay"=c(0.25 ,1.7918 ),
		"clay"=c(0.2 ,1.3218 )

   )
  ps <- ke_parms[[x$soiltexture]]
  x$Keb = ps[1] * exp(ps[2] * (x$basalcover + x$littercover) )

    # /////////////////////////////////////////////
		# //////
		# /////
		# ////
		# // Calculate weighted KE
		# // this array will be used to store the canopy cover, Ke, and Kss values for the cover types that are not 0
		x$veg = list()
		# ////
		# // Calculate KE and KSS based on vegetation type

		# // Ke and Kss for Shrubs
		x$Ke = x$Keb * 1.2
		x$veg$shrubsCoverArray = c("CanopyCover" = x$shrubscanopycover,"Ke" = x$Ke)

		#// Ke and Kss for Sod Grass
		x$Ke = x$Keb * 0.8
		x$veg$sodgrassCoverArray = c("CanopyCover" = x$sodgrasscanopycover,"Ke" = x$Ke)

    #// Ke and Kss Bunch Grass
		x$Ke = x$Keb * 1.0
		x$veg$bunchgrassCoverArray = c("CanopyCover" = x$bunchgrasscanopycover,"Ke" = x$Ke)

		#// Ke and Kss for Forbs
		x$Ke = x$Keb * 1.0
		x$veg$forbsCoverArray = c("CanopyCover" = x$forbscanopycover,"Ke" = x$Ke)

		#// Calculate the weighted Ke and Kss values based on the selected vegetation types by the user
		x$weightedKe = 0

		#// calculate weighted Ke and Kss values for the vegetation types that have non-zero values
		if(x$totalcanopycover != 0){
			for(selCanopyCover in x$veg){
				x$weightedKe = x$weightedKe + ( (selCanopyCover['CanopyCover']/x$totalcanopycover) * selCanopyCover['Ke'] )
			}
		}
		else{
			x$weightedKe = x$Keb
		}

		#/////////////////////////////////////////////
		#//////
		#/////
		#// IMPLEMENT THE NEW EQUATIONS FROM SAM FROM 01222015
		#// Kss variables
		x$Kss_Seg_Bunch = 0
		x$Kss_Seg_Sod = 0
		x$Kss_Seg_Shrub = 0
		x$Kss_Seg_Shrub_0 = 0
		x$Kss_Seg_Forbs = 0

		x$Kss_Average = 0

		x$Kss_Final = 0

		#// 1)
		#//   a) CALCULATE KSS FOR EACH VEGETATION COMMUNITY USING TOTAL FOLIAR COVER
		#//		A)   BUNCH GRASS
		if (x$totalgroundcover < 0.475){
			x$Kss_Seg_Bunch = 4.154 + 2.5535 * x$slopesteepness - 2.547 * x$totalgroundcover - 0.7822 * x$totalcanopycover
			x$Kss_Seg_Bunch = 10^x$Kss_Seg_Bunch
		}
		else{
			x$Kss_Seg_Bunch = 3.1726975 + 2.5535 * x$slopesteepness - 0.4811 * x$totalgroundcover - 0.7822 * x$totalcanopycover
			x$Kss_Seg_Bunch = 10^x$Kss_Seg_Bunch
		}

		#//		B)   SOD GRASS
		if (x$totalgroundcover < 0.475){
			x$Kss_Seg_Sod = 4.2169 + 2.5535 * x$slopesteepness - 2.547 * x$totalgroundcover - 0.7822 * x$totalcanopycover
			x$Kss_Seg_Sod = (10^x$Kss_Seg_Sod)
		}
		else{
			x$Kss_Seg_Sod = 3.2355975 + 2.5535 * x$slopesteepness - 0.4811 * x$totalgroundcover - 0.7822 * x$totalcanopycover
			x$Kss_Seg_Sod = (10^x$Kss_Seg_Sod)
		}

		#//		C)   SHRUBS
		if (x$totalgroundcover < 0.475){
			x$Kss_Seg_Shrub = 4.2587 + 2.5535 * x$slopesteepness - 2.547 * x$totalgroundcover - 0.7822 * x$totalcanopycover
			x$Kss_Seg_Shrub = (10^x$Kss_Seg_Shrub)
		}
		else{
			x$Kss_Seg_Shrub = 3.2773975 + 2.5535 * x$slopesteepness - 0.4811 * x$totalgroundcover - 0.7822 * x$totalcanopycover
			x$Kss_Seg_Shrub = (10^x$Kss_Seg_Shrub)
		}

		#//		D)   FORBS
		if (x$totalgroundcover < 0.475){
			x$Kss_Seg_Forbs = 4.1106 + 2.5535 * x$slopesteepness - 2.547 * x$totalgroundcover - 0.7822 * x$totalcanopycover
			x$Kss_Seg_Forbs = (10^x$Kss_Seg_Forbs)
		}
		else{
			x$Kss_Seg_Forbs = 3.1292975 + 2.5535 * x$slopesteepness  - 0.4811 * x$totalgroundcover - 0.7822 * x$totalcanopycover
			x$Kss_Seg_Forbs = (10^x$Kss_Seg_Forbs)
		}

		#//   b) CALCULATE KSS AT TOTAL FOLIAR = 0 FROM SHRUB EQUATION
		if (x$totalgroundcover < 0.475){
			x$Kss_Seg_Shrub_0 = 4.2587 + 2.5535 * x$slopesteepness - 2.547 * x$totalgroundcover
			x$Kss_Seg_Shrub_0 = (10^x$Kss_Seg_Shrub_0)
		}
		else{
			x$Kss_Seg_Shrub_0 = 3.2773975 + 2.5535 * x$slopesteepness - 0.4811 * x$totalgroundcover
			x$Kss_Seg_Shrub_0 = (10^x$Kss_Seg_Shrub_0)
		}

		#// 2) CALCULATE AVERAGE KSS WHEN TOTAL FOLIAR COVER IS CLOSE TO 0
		if(x$totalcanopycover > 0 & x$totalcanopycover < 0.02){
			x$Kss_Average = x$totalcanopycover/0.02 * ( (x$shrubscanopycover/x$totalcanopycover) * x$Kss_Seg_Shrub +
													 (x$sodgrasscanopycover/x$totalcanopycover) * x$Kss_Seg_Sod +
													 (x$bunchgrasscanopycover/x$totalcanopycover) * x$Kss_Seg_Bunch +
													 (x$forbscanopycover/x$totalcanopycover) * x$Kss_Seg_Forbs ) +
													 (0.02 - x$totalcanopycover)/0.02 * x$Kss_Seg_Shrub_0
		}
		else{
			x$Kss_Average = (x$shrubscanopycover/x$totalcanopycover) * x$Kss_Seg_Shrub +
						   (x$sodgrasscanopycover/x$totalcanopycover) * x$Kss_Seg_Sod +
						   (x$bunchgrasscanopycover/x$totalcanopycover) * x$Kss_Seg_Bunch +
						   (x$forbscanopycover/x$totalcanopycover) * x$Kss_Seg_Forbs
		}

		#// 3) CALCULATE KSS USED FOR RHEM (with canopy cover == 0 and canopy cover > 0)
		if(x$totalcanopycover == 0){
			if(x$totalgroundcover < 0.475){
				x$Kss_Final = 4.2587 + 2.5535 * x$slopesteepness - 2.547 * x$totalgroundcover
				x$Kss_Final = 10^x$Kss_Final
			}
			else{
				x$Kss_Final = 3.2773975 + 2.5535 * x$slopesteepness - 0.4811 * x$totalgroundcover
				x$Kss_Final = 10^x$Kss_Final
			}
		}
		else{
			if(x$totalgroundcover < 0.475){
				x$Kss_Final = x$totalgroundcover/0.475 * x$Kss_Average + (0.475 - x$totalgroundcover)/0.475 * x$Kss_Seg_Shrub
			}
			else{
				x$Kss_Final = x$Kss_Average
			}
		}

		x$Kss_Final = (x$Kss_Final * 1.3) * 2.0

		# changes units back to metric when english is selected
		if(x$units == 'english'){
			x$slopelength = x$slopelength * 0.3048
		}

		#// Set working directory and file name
		x$soilFileName = file.path( x$OUTPUT_FOLDER , paste0( x$prefix , x$scenarioname , ".par"))

		#// Write to soil log file
    x$handle = x$soilFileName
		#// soil log file
		x$timestr = Sys.time()
    x$timestamp <- format(x$timestr, '%Y-%m-%d %H:%M:%S %Z')

		#// writes the parameter file required by DRHEM
		cat(file = x$handle, "! Parameter file for scenario: " , x$scenarioname , "\n")
		cat(file = x$handle, "! Date built: " , x$timestamp ,  " (Version ", x$modelversion , ") \n", append=TRUE)
		cat(file = x$handle, "! Parameter units: DIAMS(mm), DENSITY(g/cc),TEMP(deg C) \n", append=TRUE)
		cat(file = x$handle,"BEGIN GLOBAL \n", append=TRUE)
		cat(file = x$handle, "\t\tCLEN\t=\t" , (x$slopelength*2.5) , "\n", append=TRUE, sep = '') #// The characteristic length of the hillslope in meters or feet
		cat(file = x$handle, "\t\tUNITS\t=\tmetric \n", append=TRUE)                     #// The units for the length parameter
		cat(file = x$handle, "\t\tDIAMS\t=\t" ,
			format(x$texturerow$clay_diameter, scientific = FALSE),"\t",
			format(x$texturerow$silt_diameter, scientific = FALSE),"\t",
			format(x$texturerow$small_aggregates_diameter, scientific = FALSE),"\t",
			format(x$texturerow$large_aggregates_diameter, scientific = FALSE),"\t",
			format(x$texturerow$sand_diameter, scientific = FALSE),
			"\n", append=TRUE, sep = '')      #// List of representative soil particle diameters (mm or in) for up to 5 particle classes
		cat(file = x$handle, "\t\tDENSITY\t=\t",
			format(x$texturerow$clay_specific_gravity, scientific = FALSE),"\t",
			format(x$texturerow$silt_specific_gravity, scientific = FALSE),"\t",
			format(x$texturerow$small_aggregates_specific_gravity, scientific = FALSE),"\t",
			format(x$texturerow$large_aggregates_specific_gravity, scientific = FALSE),"\t",
			format(x$texturerow$sand_specific_gravity, scientific = FALSE),
			"\n", append=TRUE, sep = '')  #// List of densities (g/cc) corresponding to the above particle classes
		cat(file = x$handle, "\t\tTEMP\t=\t40 \n", append=TRUE)                      #// temperature in degrees C
		cat(file = x$handle, "\t\tNELE\t=\t1 \n", append=TRUE)                       #// number of hillslope elements (planes)
		cat(file = x$handle, "END GLOBAL \n", append=TRUE)
		cat(file = x$handle, "BEGIN PLANE \n", append=TRUE)
		cat(file = x$handle, "\t\tID\t=\t1 \n", append=TRUE)                           #// identifier for the current plane

		cat(file = x$handle, "\t\tLEN\t=\t",x$slopelength," \n", append=TRUE)        #// The plane slope length in meters or feet
		cat(file = x$handle, "\t\tWIDTH\t=\t1.000 \n", append=TRUE)                   #// The plane bottom width in meters or feet

		x$chezy = ( (8 * 9.8)/x$ft ) ^ 0.5
		x$rchezy = ( (8 * 9.8)/x$ft )^ 0.5

		cat(file = x$handle, "\t\tCHEZY\t=\t", format(x$chezy, scientific=FALSE)," \n",  append=TRUE, sep = '')         #// Overland flow Chezy Coeff. (m^(1/2)/s) (square root meter per second)
		cat(file = x$handle, "\t\tRCHEZY\t=\t", format(x$rchezy, scientific=FALSE)," \n", append=TRUE, sep = '')        #// Concentrated flow Chezy Coeff. (m^(1/2)/s) (square root meter per second)

		x$slopeParameters = createSlopeParameters(x$slopeshape,x$slopesteepness)
		cat(file = x$handle, x$slopeParameters, append=TRUE)                               #// SL: Slope expressed as fractional rise/run
																         #// SX: Normalized distance

		cat(file = x$handle, "\t\tCV\t=\t1.0000 \n", append=TRUE)                     #// This is the coefficient of variation for Ke
		cat(file = x$handle, "\t\tSAT\t=\t",x$moisturecontent , " \n", append=TRUE)   #// Initial degree of soil saturation, expressed as a fraction of of the pore space filled
		cat(file = x$handle, "\t\tPR\t=\t1 \n", append=TRUE)                          #// Print flag
		cat(file = x$handle, "\t\tKSS\t=\t" , format(x$Kss_Final, scientific=FALSE) , " \n", append=TRUE)         #// Splash and sheet erodibility coefficient
		cat(file = x$handle, "\t\tKOMEGA\t=\t0.000007747 \n", append=TRUE)            #// Undisturbed concentrated erodibility coeff. (s2/m2) value suggested by Nearing 02Jul2014
		cat(file = x$handle, "\t\tKCM\t=\t0.000299364300 \n", append=TRUE)             #// Maximum concentrated erodibility coefficient (s2/m2)
		cat(file = x$handle, "\t\tCA\t=\t1.000 \n", append=TRUE)                      #// Cover fraction of surface covered by intercepting cover — rainfall intensity is reduced by this fraction until the specified interception depth has accumulated
		cat(file = x$handle, "\t\tIN\t=\t0.0000 \n", append=TRUE)                     #// Interception depth in mm or inches
		cat(file = x$handle, "\t\tKE\t=\t" , format(x$weightedKe, scientific=FALSE) , " \n", append=TRUE)        #// Effective hydraulic conductivity (mm/h)
		cat(file = x$handle, "\t\tG\t=\t" , format(x$meanmatricpotential, scientific=FALSE) , " \n", append=TRUE) #// Mean capillary drive, mm or inches — a zero value sets the infiltration at a constant value of Ke
		cat(file = x$handle, "\t\tDIST\t=\t" , format(x$poresizedistribution, scientific=FALSE) , " \n", append=TRUE) #// Pore size distribution index. This parameter is used for redistribution of soil moisture during unponded intervals
		cat(file = x$handle, "\t\tPOR\t=\t" , format(x$meanporosity, scientific=FALSE) , " \n", append=TRUE)      #//  Porosity
		cat(file = x$handle, "\t\tROCK\t=\t0.0000 \n", append=TRUE)                 #// Volumetric rock fraction, if any. If KE is estimated based on textural class it should be multiplied by (1 - Rock) to reflect this rock volume
		cat(file = x$handle, "\t\tSMAX\t=\t1.0000 \n", append=TRUE)                 #// Upper limit to SAT
		cat(file = x$handle, "\t\tADF\t=\t0.00 \n", append=TRUE)                       #// Beta decay factor in the detachement equation in Al-Hamdan et al 2012 (Non-FIRE)
		cat(file = x$handle, "\t\tALF\t=\t0.8000 \n", append=TRUE)                     #// allow variable alfa in the infiltration Smith-Parlange Equation, alf <= 0.05, Green and Ampt
		cat(file = x$handle, "\t\tBARE\t=\t0.23 \n", append=TRUE)                   #// Fraction of bare soil to total area. 1 - ground cover ( this will be used if ADF is not 0)
		cat(file = x$handle, "\t\tRSP\t=\t1.000 \n", append=TRUE)                      #// Rill spacing in meters or feet
		cat(file = x$handle, "\t\tSPACING\t=\t1.000 \n", append=TRUE)		             #// Average micro topographic spacing in meters or feet
		cat(file = x$handle, "\t\tFRACT\t=\t" ,
							format(x$texturerow$clay_fraction, scientific=FALSE)  , "\t" ,
							format(x$texturerow$silt_fraction, scientific=FALSE) , "\t" ,
							format(x$texturerow$small_aggregates_fraction, scientific=FALSE) , "\t" ,
							format(x$texturerow$large_aggregates_fraction, scientific=FALSE) , "\t" ,
							format(x$texturerow$sand_fraction, scientific=FALSE) ,
							"\n", append=TRUE) #// List of particle class fractions — must sum to one
		cat(file = x$handle, "END PLANE \n", append=TRUE)


		return (x)
	}


#' Run RHEM in batch mode from excel
#'
#' @param xlsfile An excel file with a tab named 'Inputs'
#' @param output folder to save outputs
#' @param exe path to executable
#' @param clifile path to storm file
#' @param ... named arguments to override batchfile properties
#' @details Function will look for rhem executable in same folder as xlsfile.
#' @examples
#' rhem_batch('RHEM_Template.xlsx')
rhem_batch <- function(xlsfile, output = './output', exe ='', cliFile ='', ...){
    require(readxl)

    xtras <- list(...)

    #checks

    if(!dir.exists(output)) dir.create(output, recursive = TRUE)

    if(exe == '') {
        exe <- list.files(pattern = 'rhem.*?exe')[1]
    }

    if(!file.exists(exe)){
        stop( 'cant find rhem executable ', exe )
    }

    if(!file.exists(cliFile)){
        stop( 'cant find cliFile ', cliFile )
    }

    # read template file
    sh <- excel_sheets(xlsfile)

    input <- grep('Inputs', sh)

    if(length(input) == 0 | length(input) > 1){
        stop('Cant find single tab in ', xlsfile, ' named "Inputs" ')
    }

    dat <- read_excel(xlsfile, sheet = input)
    dat <- as.data.frame(dat)

    # loop through rows, generating PAR files
      # then running and saving the outputs

    setname <- function(x, old, new){ names(x)[which(names(x) == old)] <- new; x }

    Z <- list()

    for( r in 1:nrow(dat) ){

        x <- dat[r,]

        x <- setname(x, "Scenario Name", "scenarioname")
        x <- setname(x, "Units", "units")
        x <- setname(x, "Soil Texture", "soiltexture")
        x <- setname(x, "Slope Length (meters)", "slopelength")
        x <- setname(x, "Slope Shape (Uniform, S-Shaped, Convex, Concave)", "slopeshape")
        x <- setname(x, "Slope Steepness ( % )", "slopesteepness")
        x <- setname(x, "Bunch Grass ( % )", "bunchgrasscanopycover")
        x <- setname(x, "Forbs/Annuals ( %)", "forbscanopycover")
        x <- setname(x, "Shrubs ( % )", "shrubscanopycover")
        x <- setname(x, "Sod Grass (%)", "sodgrasscanopycover")
        x <- setname(x, "Basal Plant Cover ( % )", "basalcover")
        x <- setname(x, "Rock Cover ( % )", "rockcover")
        x <- setname(x, "Litter Cover  ( % )", "littercover")
        x <- setname(x, "Biological Crusts Cover            ( % )", "cryptogamscover")

        for(j in names(xtras)){
            x[[j]] <- xtras[[j]]
        }

        x$OUTPUT_FOLDER <- output

        p <- build_par(x)

        # now fit
        cat('running ', p$soilFileName, '\n'); flush.console()
        result <- run_rhem(p, cliFile, exe, output)
        cat(result$resp, '\n'); flush.console()


        dat[r,"Avg Runoff (mm/year)"] <- result$averages[[1]]
        dat[r,"Avg Soil Loss (ton/ha/year)"] <- result$averages[[2]]
        dat[r,"Avg SY (ton/ha/year)"] <- result$averages[[3]]

        Z[[r]] <- result
    }

    write.csv( dat, file.path(dirname(outfile), paste0('output_',p$prefix, '.csv')), row.names = F)
    return(Z)

}

