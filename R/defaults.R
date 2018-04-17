#' Default parameters
#'
#' @return A named list of default parameters used for building the RHEM .par file
#' @examples
#' par_defaults()
par_defaults <- function() {
  list(
    scenarioname = as.numeric(Sys.time()),
    units = 'Metric',
    soiltexture = 'sandy loam',
    moisturecontent = 25,
    bunchgrasscanopycover = 10,
    forbscanopycover = 5,
    shrubscanopycover = 15,
    sodgrasscanopycover = 0,
    rockcover = 10,
    basalcover = 20,
    littercover = 20,
    cryptogamscover = 10,
    slopelength = 50,
    slopeshape = 'uniform',
    slopesteepness = 20,
    version = 'x',
    OUTPUT_FOLDER = '.'
  )
}



#' Soil Texture parameters
#'
#' @return A data.frame with each row corresponding to a USDA soil texture class and each column representing a parameter used for building the RHEM .par file
#' @examples
#' texture_df()
texture_df <- function(){

	structure(list(class = c("clay", "clay loam", "loam", "Loamy Sand", 
	"sand", "sandy clay", "sandy clay loam", "sandy loam", "silt", 
	"silt loam", "silty clay", "silty clay loam"), clay_diameter = c(0.002, 
	0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 
	0.002, 0.002), silt_diameter = c(0.01, 0.01, 0.01, 0.01, 0.01, 
	0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01), small_aggregates_diameter = c(0.0696, 
	0.038, 0.03, 0.03, 0.03, 0.0588, 0.03, 0.03, 0.03, 0.03, 0.0636, 
	0.0388), large_aggregates_diameter = c(0.896, 0.58, 0.3, 0.3, 
	0.3, 0.788, 0.406, 0.3, 0.3, 0.3, 0.836, 0.588), sand_diameter = c(0.2, 
	0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2), clay_specific_gravity = c(2.6, 
	2.6, 2.6, 2.6, 2.6, 2.6, 2.6, 2.6, 2.6, 2.6, 2.6, 2.6), silt_specific_gravity = c(2.65, 
	2.65, 2.65, 2.65, 2.65, 2.65, 2.65, 2.65, 2.65, 2.65, 2.65, 2.65
	), small_aggregates_specific_gravity = c(1.8, 1.8, 1.8, 1.8, 
	1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8), large_aggregates_specific_gravity = c(1.6, 
	1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6), sand_specific_gravity = c(2.65, 
	2.65, 2.65, 2.65, 2.65, 2.65, 2.65, 2.65, 2.65, 2.65, 2.65, 2.65
	), KE = c(0.29687343039536, 0.94841921987025, 3.9470925832243, 
	13.276540613363, 27.929049600959, 0.54369911466242, 1.4676609893289, 
	7.1333176415294, 2.1182389890158, 2.1182389890158, 0.41736011698005, 
	0.73383049466444), mean_matric_potential = c(400L, 260L, 110L, 
	70L, 50L, 300L, 260L, 130L, 200L, 200L, 380L, 350L), pore_size_distribution_index = c(0.16, 
	0.24, 0.25, 0.55, 0.69, 0.22, 0.32, 0.38, 0.23, 0.23, 0.15, 0.18
	), mean_porosity = c(0.4724, 0.4589, 0.4531, 0.4087, 0.3902, 
	0.4146, 0.4377, 0.4306, 0.4258, 0.4455, 0.4704, 0.4581), clay_fraction = c(0.1247, 
	0.0848, 0.0498, 0.0137, 0.0077, 0.1073, 0.0641, 0.0325, 0.0221, 
	0.0505, 0.1196, 0.0861), silt_fraction = c(1e-04, 0.0397, 0.1128, 
	0.0353, 0.018, 1e-04, 5e-04, 0.0542, 0.682, 0.3095, 0.1517, 0.1986
	), small_aggregates_fraction = c(0.2567, 0.3157, 0.2877, 0.0951, 
	0.0435, 0.1039, 0.1686, 0.1801, 0.153, 0.3497, 0.3244, 0.4014
	), large_aggregates_fraction = c(0.6057, 0.5148, 0.4004, 0.233, 
	0.148, 0.7544, 0.6217, 0.3939, 0.0916, 0.2255, 0.4012, 0.3044
	), sand_fraction = c(0.0128, 0.045, 0.1494, 0.6229, 0.7827, 0.0344, 
	0.1451, 0.3394, 0.0513, 0.0647, 0.0031, 0.0096)), .Names = c("class", 
	"clay_diameter", "silt_diameter", "small_aggregates_diameter", 
	"large_aggregates_diameter", "sand_diameter", "clay_specific_gravity", 
	"silt_specific_gravity", "small_aggregates_specific_gravity", 
	"large_aggregates_specific_gravity", "sand_specific_gravity", 
	"KE", "mean_matric_potential", "pore_size_distribution_index", 
	"mean_porosity", "clay_fraction", "silt_fraction", "small_aggregates_fraction", 
	"large_aggregates_fraction", "sand_fraction"), row.names = c(NA, 
	-12L), class = "data.frame")
}

#' Retrieve soil texture parameters
#'
#' @param texture character
#' @details texture may be one of "clay", "clay loam", "loam", "Loamy Sand", "sand", "sandy clay", "sandy clay loam", "sandy loam", "silt","silt loam", "silty clay", "silty clay loam"
#' @return named list of input parameters for given soil type
#' @examples
#' get_soil_texture('sandy loam')
#'
get_soil_texture <- function(texture) {

  tx <- texture_df()
  ti <- which(tx$class == texture)
  if(length(ti) < 0 ) stop( 'texture class ', texture, ' not recognized')

 return( tx[ti,] )

}

