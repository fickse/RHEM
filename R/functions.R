

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

  SLline <- paste0("\t\tSL\t= ", paste(format(round(SL,2), nsmall = 2), collapse = '\t,\t'))
  SXline <- paste0("\t\tSX\t= ", paste(format(round(SX,2), nsmall = 2), collapse = '\t,\t'))

  return( paste0(SLline, '\n', SXline))
 }





#' Write .par file for input to rhem
#'
#' @param ... named arguments. see \code{details}
#' @details the complete list of input parameters may be found via the \code{\link{par_defaults}} function. Parameters include:
#'  scenarioname: (defaults to \code{as.numeric(Sys.time())})
#'  units: 'Metric' or 'English'
#'  soiltexture: see \code{\link{texture_df}}
#'  moisturecontent: Initial moisture content % saturation ( default = 25)
#'  bunchgrasscanopycover: integer (%)
#'  forbscanopycover: integer (%)
#'  shrubscanopycover: integer (%)
#'  sodgrasscanopycover: integer (%)
#'  rockcover: integer (%)
#'  basalcover: integer (%)
#'  littercover: integer (%)
#'  cryptogamscover: integer (%)
#'  slopelength : integer (m)
#'  slopeshape : "uniform", "convex", "concave" or "s-shaped"
#'  slopesteepness : integer (%)
#'  version : character (currently no effect)
#'  OUTPUT_FOLDER : place to save .par file. Defaults to '.'
#' @return list of inputs for .par file
#' @examples
#' a <- build_par() # defaults
#' a
#' unlink(a$handle)
#'
build_par <- function(...){

  y <- list(...)

  x <- par_defaults()

  for( n in names(y)){

    if (! n %in% names(x)) stop('variable name ', n, ' not recognized. See parameter defaults with par_defaults')

    x[[n]] <- y[[n]]

  }

  # quality control checks here...


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

    "Sand" = c(24,0.3483),
		"Loamy Sand"=c(10 ,0.8755 ),
		"Sandy Loam"=c(5 ,1.1632 ),
		"Loam"=c(2.5 ,1.5686 ),
		"Silt Loam"=c(1.2 ,2.0149 ),
		"Silt"=c(1.2 ,2.0149 ),
		"Sandy Clay Loam"=c(0.80 ,2.1691 ),
		"Clay Loam"=c(0.50 ,2.3026 ),
		"Silty Clay Loam"=c(0.40 ,2.1691 ),
		"Sandy Clay"=c(0.30 ,2.1203 ),
		"Silty Clay"=c(0.25 ,1.7918 ),
		"Clay"=c(0.2 ,1.3218 )

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
		x$soilFileName = file.path( x$OUTPUT_FOLDER , paste0( "scenario_input_" , x$scenarioname , ".par"))

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
		cat(file = x$handle, "		CLEN	=	" , (x$slopelength*2.5) , " \n", append=TRUE) #// The characteristic length of the hillslope in meters or feet
		cat(file = x$handle, "		UNITS	=	metric \n", append=TRUE)                     #// The units for the length parameter
		cat(file = x$handle, "		DIAMS	=	" , x$texturerow$clay_diameter,"\t",x$texturerow$silt_diameter,"\t",x$texturerow$small_aggregates_diameter,"\t",x$texturerow$large_aggregates_diameter,"\t",x$texturerow$sand_diameter,"\n", append=TRUE)      #// List of representative soil particle diameters (mm or in) for up to 5 particle classes
		cat(file = x$handle, "		DENSITY	=	",x$texturerow$clay_specific_gravity,"\t",x$texturerow$silt_specific_gravity,"\t",x$texturerow$small_aggregates_specific_gravity,"\t",x$texturerow$large_aggregates_specific_gravity,"\t",x$texturerow$sand_specific_gravity,"\n", append=TRUE)  #// List of densities (g/cc) corresponding to the above particle classes
		cat(file = x$handle, "		TEMP	=	40 \n", append=TRUE)                      #// temperature in degrees C
		cat(file = x$handle, "		NELE	=	1 \n", append=TRUE)                       #// number of hillslope elements (planes)
		cat(file = x$handle, "END GLOBAL \n", append=TRUE)
		cat(file = x$handle, "BEGIN PLANE \n", append=TRUE)
		cat(file = x$handle, "		ID	=	1 \n", append=TRUE)                           #// identifier for the current plane

		cat(file = x$handle, "		LEN	=	",x$slopelength," \n", append=TRUE)        #// The plane slope length in meters or feet
		cat(file = x$handle, "		WIDTH	=	1.000 \n", append=TRUE)                   #// The plane bottom width in meters or feet

		x$chezy = ( (8 * 9.8)/x$ft ) ^ 0.5
		x$rchezy = ( (8 * 9.8)/x$ft )^ 0.5

		cat(file = x$handle, "		CHEZY	=	",x$chezy," \n", append=TRUE)         #// Overland flow Chezy Coeff. (m^(1/2)/s) (square root meter per second)
		cat(file = x$handle, "		RCHEZY	=	",x$rchezy," \n", append=TRUE)        #// Concentrated flow Chezy Coeff. (m^(1/2)/s) (square root meter per second)

		x$slopeParameters = createSlopeParameters(x$units,x$slopelength,x$slopeshape,x$slopesteepness)
		cat(file = x$handle, x$slopeParameters, append=TRUE)                               #// SL: Slope expressed as fractional rise/run
																         #// SX: Normalized distance

		cat(file = x$handle, "		CV	=	1.0000 \n", append=TRUE)                     #// This is the coefficient of variation for Ke
		cat(file = x$handle, "		SAT	=	",x$moisturecontent , " \n", append=TRUE)   #// Initial degree of soil saturation, expressed as a fraction of of the pore space filled
		cat(file = x$handle, "		PR	=	1 \n", append=TRUE)                          #// Print flag
		cat(file = x$handle, "		KSS	=	" , x$Kss_Final , " \n", append=TRUE)         #// Splash and sheet erodibility coefficient
		cat(file = x$handle, "		KOMEGA	=	0.000007747 \n", append=TRUE)            #// Undisturbed concentrated erodibility coeff. (s2/m2) value suggested by Nearing 02Jul2014
		cat(file = x$handle, "		KCM	=	0.000299364300 \n", append=TRUE)             #// Maximum concentrated erodibility coefficient (s2/m2)
		cat(file = x$handle, "		CA	=	1.000 \n", append=TRUE)                      #// Cover fraction of surface covered by intercepting cover — rainfall intensity is reduced by this fraction until the specified interception depth has accumulated
		cat(file = x$handle, "		IN	=	0.0000 \n", append=TRUE)                     #// Interception depth in mm or inches
		cat(file = x$handle, "		KE	=	" , x$weightedKe , " \n", append=TRUE)        #// Effective hydraulic conductivity (mm/h)
		cat(file = x$handle, "		G	=	" , x$meanmatricpotential , " \n", append=TRUE) #// Mean capillary drive, mm or inches — a zero value sets the infiltration at a constant value of Ke
		cat(file = x$handle, "		DIST	=	" , x$poresizedistribution , " \n", append=TRUE) #// Pore size distribution index. This parameter is used for redistribution of soil moisture during unponded intervals
		cat(file = x$handle, "		POR	=	" , x$meanporosity , " \n", append=TRUE)      #//  Porosity
		cat(file = x$handle, "		ROCK	=	0.0000 \n", append=TRUE)                 #// Volumetric rock fraction, if any. If KE is estimated based on textural class it should be multiplied by (1 - Rock) to reflect this rock volume
		cat(file = x$handle, "		SMAX	=	1.0000 \n", append=TRUE)                 #// Upper limit to SAT
		cat(file = x$handle, "		ADF	=	0.00 \n", append=TRUE)                       #// Beta decay factor in the detachement equation in Al-Hamdan et al 2012 (Non-FIRE)
		cat(file = x$handle, "		ALF	=	0.8000 \n", append=TRUE)                     #// allow variable alfa in the infiltration Smith-Parlange Equation, alf <= 0.05, Green and Ampt
		cat(file = x$handle, "		BARE	=	0.23 \n", append=TRUE)                   #// Fraction of bare soil to total area. 1 - ground cover ( this will be used if ADF is not 0)
		cat(file = x$handle, "		RSP	=	1.000 \n", append=TRUE)                      #// Rill spacing in meters or feet
		cat(file = x$handle, "		SPACING	=	1.000 \n", append=TRUE)		             #// Average micro topographic spacing in meters or feet
		cat(file = x$handle, "		FRACT	=	" ,  x$texturerow$clay_fraction  , "\t" , x$texturerow$silt_fraction , "\t" , x$texturerow$small_aggregates_fraction , "\t" , x$texturerow$large_aggregates_fraction , "\t" , x$texturerow$sand_fraction , "\n", append=TRUE) #// List of particle class fractions — must sum to one
		cat(file = x$handle, "END PLANE \n", append=TRUE)


		return (x)
	}





