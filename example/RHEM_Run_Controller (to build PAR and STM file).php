<?php
/**
 * This is part of the Run RHEM controller which is used the build all the input files and to run the RHEM model.
 *
 */

		$Ke = $this->buildParametersFile($scenarioname,$units,$soiltexture,$moisturecontent,$bunchgrasscanopycover,$forbscanopycover,$shrubscanopycover,$sodgrasscanopycover,
											 $rockcover, $basalcover, $littercover,$cryptogamscover,$slopelength,$slopeshape,$slopesteepness, $scenariodata['version'] );

		// Ke from the Soil file calculations will be used to exclude precipitation dates
		$this->buildStormFile($scenarioname,$stateid,$climatestation, $Ke, $scenariodata['version']);



	/**
	 * Builds the parameters file. This is the main parameter file to run the RHEM model.
	 *
	 */
	function buildParametersFile($scenarioname,$units,$soiltexture,$moisturecontent,$bunchgrasscanopycover,$forbscanopycover,$shrubscanopycover,$sodgrasscanopycover,
								 $rockcover, $basalcover, $littercover,$cryptogamscover,$slopelength,$slopeshape,$slopesteepness, $modelversion)
	{
		// convert to percent values
		$bunchgrasscanopycover = $bunchgrasscanopycover/100;
		$forbscanopycover = $forbscanopycover/100;
		$shrubscanopycover = $shrubscanopycover/100;
		$sodgrasscanopycover = $sodgrasscanopycover/100;

		// canopy cover for grass (this is for the new Kss equations from Sam)
		$grasscanopycover = $bunchgrasscanopycover + $forbscanopycover + $sodgrasscanopycover;

		$moisturecontent = $moisturecontent/100;

		////
		// TOTAL CANOPY COVER
		$totalcanopycover = $bunchgrasscanopycover + $forbscanopycover + $shrubscanopycover + $sodgrasscanopycover;

		$rockcover = $rockcover/100;
		$basalcover = $basalcover/100;
		$littercover = $littercover/100;
		$cryptogamscover = $cryptogamscover/100;

		////
		// TOTAL GROUND COVER
		$totalgroundcover = $basalcover + $littercover + $cryptogamscover + $rockcover;

		$slopesteepness = $slopesteepness/100;

		// get the soil information from the database
		$texturerow = $this->Rhemmodel->get_soil_texture($soiltexture);
		$meanclay = $texturerow->mean_clay;

		$meanmatricpotential = $texturerow->mean_matric_potential;
		$poresizedistribution = $texturerow->pore_size_distribution;

		$meanporosity = $texturerow->mean_porosity;	
		
		// compute ft (replaces fe and fr)
		$ft =  ( -1 * 0.109) + (1.425 * $littercover) + (0.442 * $rockcover) + (1.764 * ($basalcover + $cryptogamscover)) + (2.068 * $slopesteepness);
		$ft = pow(10,$ft);


		// Implement the new equations to calculate Ke.
		switch ($soiltexture) {
			case 1: // "Sand"
				$Keb = 24 * exp(0.3483 * ($basalcover + $littercover) );
				break;
			case 2: // "Loamy Sand"
				$Keb = 10 * exp(0.8755 * ($basalcover + $littercover) );
				break;
			case 3: // "Sandy Loam"
				$Keb = 5 * exp(1.1632 * ($basalcover + $littercover) );
				break;
			case 4: // "Loam"
				$Keb = 2.5 * exp(1.5686 * ($basalcover + $littercover) );
				break;
			case 5: // "Silt Loam"	
				$Keb = 1.2 * exp(2.0149 * ($basalcover + $littercover) );
				break;
			case 6: // "Silt" (there is no equation devoped, yet, for silt)
				$Keb = 1.2 * exp(2.0149 * ($basalcover + $littercover) );
				break;
			case 7: // "Sandy Clay Loam"
				$Keb = 0.80 * exp(2.1691 * ($basalcover + $littercover) );
				break;
			case 8: // "Clay Loam"
				$Keb = 0.50 * exp(2.3026 * ($basalcover + $littercover) );
				break;
			case 9: // "Silty Clay Loam"
				$Keb = 0.40 * exp(2.1691 * ($basalcover + $littercover) );
				break;
			case 10: // "Sandy Clay"
				$Keb = 0.30 * exp(2.1203 * ($basalcover + $littercover) );
				break;
			case 11: // "Silty Clay"
				$Keb = 0.25 * exp(1.7918 * ($basalcover + $littercover) );
				break;
			case 12: // "Clay"
				$Keb = 0.2 * exp(1.3218 * ($basalcover + $littercover) );
				break;
		}


		/////////////////////////////////////////////
		////// 
		/////
		////
		// Calculate weighted KE
		// this array will be used to store the canopy cover, Ke, and Kss values for the cover types that are not 0
		$vegetationCanopyCoverArray = array();
		//// 
		// Calculate KE and KSS based on vegetation type

		// Ke and Kss for Shrubs
		$Ke = $Keb * 1.2;
		$shrubsCoverArray = array("CanopyCover" => $shrubscanopycover,"Ke" => $Ke);
		array_push($vegetationCanopyCoverArray,$shrubsCoverArray);

		// Ke and Kss for Sod Grass
		$Ke = $Keb * 0.8;
		$sodgrassCoverArray = array("CanopyCover" => $sodgrasscanopycover,"Ke" => $Ke);
		array_push($vegetationCanopyCoverArray,$sodgrassCoverArray);

		// Ke and Kss Bunch Grass
		$Ke = $Keb * 1.0;
		$bunchgrassCoverArray = array("CanopyCover" => $bunchgrasscanopycover,"Ke" => $Ke);
		array_push($vegetationCanopyCoverArray,$bunchgrassCoverArray);

		// Ke and Kss for Forbs
		$Ke = $Keb * 1.0;
		$forbsCoverArray = array("CanopyCover" => $forbscanopycover,"Ke" => $Ke);
		array_push($vegetationCanopyCoverArray,$forbsCoverArray);

		// Calculate the weighted Ke and Kss values based on the selected vegetation types by the user
		$weightedKe = 0;

		// calculate weighted Ke and Kss values for the vegetation types that have non-zero values
		if($totalcanopycover != 0){
			foreach($vegetationCanopyCoverArray as $selCanopyCover){
				$weightedKe = $weightedKe + ( ($selCanopyCover['CanopyCover']/$totalcanopycover) * $selCanopyCover['Ke'] );
			}
		}
		else{
			$weightedKe = $Keb;
		}

		/////////////////////////////////////////////
		////// 
		/////
		// IMPLEMENT THE NEW EQUATIONS FROM SAM FROM 01222015
		// Kss variables
		$Kss_Seg_Bunch = 0;
		$Kss_Seg_Sod = 0;
		$Kss_Seg_Shrub = 0;
		$Kss_Seg_Shrub_0 = 0;
		$Kss_Seg_Forbs = 0;

		$Kss_Average = 0;

		$Kss_Final = 0;

		// 1) 
		//   a) CALCULATE KSS FOR EACH VEGETATION COMMUNITY USING TOTAL FOLIAR COVER
		//		A)   BUNCH GRASS
		if ($totalgroundcover < 0.475){
			$Kss_Seg_Bunch = 4.154 + 2.5535 * $slopesteepness - 2.547 * $totalgroundcover - 0.7822 * $totalcanopycover;
			$Kss_Seg_Bunch = pow(10,$Kss_Seg_Bunch);
		}
		else{
			$Kss_Seg_Bunch = 3.1726975 + 2.5535 * $slopesteepness - 0.4811 * $totalgroundcover - 0.7822 * $totalcanopycover;
			$Kss_Seg_Bunch = pow(10,$Kss_Seg_Bunch);
		}

		//		B)   SOD GRASS
		if ($totalgroundcover < 0.475){
			$Kss_Seg_Sod = 4.2169 + 2.5535 * $slopesteepness - 2.547 * $totalgroundcover - 0.7822 * $totalcanopycover;
			$Kss_Seg_Sod = pow(10,$Kss_Seg_Sod);
		}
		else{
			$Kss_Seg_Sod = 3.2355975 + 2.5535 * $slopesteepness - 0.4811 * $totalgroundcover - 0.7822 * $totalcanopycover;
			$Kss_Seg_Sod = pow(10,$Kss_Seg_Sod);
		}

		//		C)   SHRUBS
		if ($totalgroundcover < 0.475){
			$Kss_Seg_Shrub = 4.2587 + 2.5535 * $slopesteepness - 2.547 * $totalgroundcover - 0.7822 * $totalcanopycover;
			$Kss_Seg_Shrub = pow(10,$Kss_Seg_Shrub);
		}
		else{
			$Kss_Seg_Shrub = 3.2773975 + 2.5535 * $slopesteepness - 0.4811 * $totalgroundcover - 0.7822 * $totalcanopycover;
			$Kss_Seg_Shrub = pow(10,$Kss_Seg_Shrub);
		}

		//		D)   FORBS
		if ($totalgroundcover < 0.475){
			$Kss_Seg_Forbs = 4.1106 + 2.5535 * $slopesteepness - 2.547 * $totalgroundcover - 0.7822 * $totalcanopycover;
			$Kss_Seg_Forbs = pow(10,$Kss_Seg_Forbs);
		}
		else{
			$Kss_Seg_Forbs = 3.1292975 + 2.5535 * $slopesteepness  - 0.4811 * $totalgroundcover - 0.7822 * $totalcanopycover;
			$Kss_Seg_Forbs = pow(10,$Kss_Seg_Forbs);
		}

		//   b) CALCULATE KSS AT TOTAL FOLIAR = 0 FROM SHRUB EQUATION
		if ($totalgroundcover < 0.475){
			$Kss_Seg_Shrub_0 = 4.2587 + 2.5535 * $slopesteepness - 2.547 * $totalgroundcover;
			$Kss_Seg_Shrub_0 = pow(10,$Kss_Seg_Shrub_0);
		}
		else{
			$Kss_Seg_Shrub_0 = 3.2773975 + 2.5535 * $slopesteepness - 0.4811 * $totalgroundcover;
			$Kss_Seg_Shrub_0 = pow(10,$Kss_Seg_Shrub_0);
		}

		// 2) CALCULATE AVERAGE KSS WHEN TOTAL FOLIAR COVER IS CLOSE TO 0
		if($totalcanopycover > 0 && $totalcanopycover < 0.02){
			$Kss_Average = $totalcanopycover/0.02 * ( ($shrubscanopycover/$totalcanopycover) * $Kss_Seg_Shrub + 
													 ($sodgrasscanopycover/$totalcanopycover) * $Kss_Seg_Sod + 
													 ($bunchgrasscanopycover/$totalcanopycover) * $Kss_Seg_Bunch + 
													 ($forbscanopycover/$totalcanopycover) * $Kss_Seg_Forbs ) + 
													 (0.02 - $totalcanopycover)/0.02 * $Kss_Seg_Shrub_0; 
		}
		else{
			$Kss_Average = ($shrubscanopycover/$totalcanopycover) * $Kss_Seg_Shrub + 
						   ($sodgrasscanopycover/$totalcanopycover) * $Kss_Seg_Sod +
						   ($bunchgrasscanopycover/$totalcanopycover) * $Kss_Seg_Bunch +
						   ($forbscanopycover/$totalcanopycover) * $Kss_Seg_Forbs;
		}

		// 3) CALCULATE KSS USED FOR RHEM (with canopy cover == 0 and canopy cover > 0)
		if($totalcanopycover == 0){
			if($totalgroundcover < 0.475){
				$Kss_Final = 4.2587 + 2.5535 * $slopesteepness - 2.547 * $totalgroundcover;
				$Kss_Final = pow(10,$Kss_Final);
			}
			else{
				$Kss_Final = 3.2773975 + 2.5535 * $slopesteepness - 0.4811 * $totalgroundcover;
				$Kss_Final = pow(10,$Kss_Final);
			}
		}
		else{
			if($totalgroundcover < 0.475){
				$Kss_Final = $totalgroundcover/0.475 * $Kss_Average + (0.475 - $totalgroundcover)/0.475 * $Kss_Seg_Shrub;
			}
			else{
				$Kss_Final = $Kss_Average;
			}
		}

		$Kss_Final = ($Kss_Final * 1.3) * 2.0;

		# changes units back to metric when english is selected
		if($units == 'english'){
			$slopelength = $slopelength * 0.3048;
		}

		// Set working directory and file name
		$soilFileName = OUTPUT_FOLDER . "scenario_input_" . $this->session->userdata('user_id') . "_" . $this->session->userdata('scenarioid') . ".par";

		// Write to soil log file
		$handle = fopen($soilFileName, "w");		

		// soil log file
		$timestr = date("F j, Y, g:i a");

		// writes the parameter file required by DRHEM 
		fwrite($handle, "! Parameter file for scenario: " . $scenarioname . "\n");
		fwrite($handle, "! Date built: " . $timestr .  " (Version ". $modelversion . ") \n");
		fwrite($handle, "! Parameter units: DIAMS(mm), DENSITY(g/cc),TEMP(deg C) \n");
		fwrite($handle,"BEGIN GLOBAL \n");
		fwrite($handle, "		CLEN	=	" . ($slopelength*2.5) . " \n"); // The characteristic length of the hillslope in meters or feet
		fwrite($handle, "		UNITS	=	metric \n");                     // The units for the length parameter
		fwrite($handle, "		DIAMS	=	" . $texturerow->clay_diameter . "\t" . $texturerow->silt_diameter . "\t" . $texturerow->small_aggregates_diameter . "\t" . $texturerow->large_aggregates_diameter . "\t" . $texturerow->sand_diameter . "\n");      // List of representative soil particle diameters (mm or in) for up to 5 particle classes
		fwrite($handle, "		DENSITY	=	" . $texturerow->clay_specific_gravity . "\t" . $texturerow->silt_specific_gravity . "\t" . $texturerow->small_aggregates_specific_gravity . "\t" . $texturerow->large_aggregates_specific_gravity . "\t" . $texturerow->sand_specific_gravity . "\n");  // List of densities (g/cc) corresponding to the above particle classes
		fwrite($handle, "		TEMP	=	40 \n");                      // temperature in degrees C 
		fwrite($handle, "		NELE	=	1 \n");                       // number of hillslope elements (planes)
		fwrite($handle, "END GLOBAL \n");                  
		fwrite($handle, "BEGIN PLANE \n");                               
		fwrite($handle, "		ID	=	1 \n");                           // identifier for the current plane
		
		fwrite($handle, "		LEN	=	" . $slopelength . " \n");        // The plane slope length in meters or feet
		fwrite($handle, "		WIDTH	=	1.000 \n");                   // The plane bottom width in meters or feet

		$chezy = pow( ( (8 * 9.8)/$ft ), 0.5 );
		$rchezy = pow( ( (8 * 9.8)/$ft ), 0.5 );

		fwrite($handle, "		CHEZY	=	" . $chezy . " \n");         // Overland flow Chezy Coeff. (m^(1/2)/s) (square root meter per second)
		fwrite($handle, "		RCHEZY	=	" . $rchezy . " \n");        // Concentrated flow Chezy Coeff. (m^(1/2)/s) (square root meter per second)
		
		$slopeParameters = $this->createSlopeParameters($units,$slopelength,$slopeshape,$slopesteepness);
		fwrite($handle, $slopeParameters);                               // SL: Slope expressed as fractional rise/run
																         // SX: Normalized distance
			  
		fwrite($handle, "		CV	=	1.0000 \n");                     // This is the coefficient of variation for Ke
		fwrite($handle, "		SAT	=	" . $moisturecontent . " \n");   // Initial degree of soil saturation, expressed as a fraction of of the pore space filled
		fwrite($handle, "		PR	=	1 \n");                          // Print flag
		fwrite($handle, "		KSS	=	" . $Kss_Final . " \n");         // Splash and sheet erodibility coefficient
		fwrite($handle, "		KOMEGA	=	0.000007747 \n");            // Undisturbed concentrated erodibility coeff. (s2/m2) value suggested by Nearing 02Jul2014  
		fwrite($handle, "		KCM	=	0.000299364300 \n");             // Maximum concentrated erodibility coefficient (s2/m2) 
		fwrite($handle, "		CA	=	1.000 \n");                      // Cover fraction of surface covered by intercepting cover — rainfall intensity is reduced by this fraction until the specified interception depth has accumulated
		fwrite($handle, "		IN	=	0.0000 \n");                     // Interception depth in mm or inches
		fwrite($handle, "		KE	=	" . $weightedKe . " \n");        // Effective hydraulic conductivity (mm/h)
		fwrite($handle, "		G	=	" . $meanmatricpotential . " \n"); // Mean capillary drive, mm or inches — a zero value sets the infiltration at a constant value of Ke
		fwrite($handle, "		DIST	=	" . $poresizedistribution . " \n"); // Pore size distribution index. This parameter is used for redistribution of soil moisture during unponded intervals
		fwrite($handle, "		POR	=	" . $meanporosity . " \n");      //  Porosity
		fwrite($handle, "		ROCK	=	0.0000 \n");                 // Volumetric rock fraction, if any. If KE is estimated based on textural class it should be multiplied by (1 - Rock) to reflect this rock volume
		fwrite($handle, "		SMAX	=	1.0000 \n");                 // Upper limit to SAT
		fwrite($handle, "		ADF	=	0.00 \n");                       // Beta decay factor in the detachement equation in Al-Hamdan et al 2012 (Non-FIRE)
		fwrite($handle, "		ALF	=	0.8000 \n");                     // allow variable alfa in the infiltration Smith-Parlange Equation, alf <= 0.05, Green and Ampt  
		fwrite($handle, "		BARE	=	0.23 \n");                   // Fraction of bare soil to total area. 1 - ground cover ( this will be used if ADF is not 0)
		fwrite($handle, "		RSP	=	1.000 \n");                      // Rill spacing in meters or feet
		fwrite($handle, "		SPACING	=	1.000 \n");		             // Average micro topographic spacing in meters or feet
		fwrite($handle, "		FRACT	=	" .  $texturerow->clay_fraction  . "\t" . $texturerow->silt_fraction . "\t" . $texturerow->small_aggregates_fraction . "\t" . $texturerow->large_aggregates_fraction . "\t" . $texturerow->sand_fraction . "\n"); // List of particle class fractions — must sum to one
		fwrite($handle, "END PLANE \n");
		
		// Closes the soil file
		fclose($handle);

		return $weightedKe;
	}

	
	/**
	 * Builds the storm file based on the selected or modified Cligen PAR file.
	 *
	 */
	function  buildStormFile($scenarioname,$stateid,$climatestation,$Ke, $modelversion)
	{
		// Set working directory and file name
		$stormFileName = OUTPUT_FOLDER . "storm_input_" . $this->session->userdata('user_id') . "_" . $this->session->userdata('scenarioid') . ".pre";
		$timestr = date("F j, Y, g:i a");

		// Write the precip file to the file.  This is done first in order to get the total number of rain events. 
		$numevents = $this->extractPrecipFromCligenOutput($scenarioname,$stateid,$climatestation,$Ke,$stormFileName);

		// Re-open the storm file and append the scenario information at the beginning of the file
		$storm_content = file_get_contents($stormFileName);
		$handle = fopen($stormFileName, "w");
		fwrite($handle, "# Storm file for scenario: " . $scenarioname . "\n");
		fwrite($handle, "# Date built: " . $timestr .  " (Version ". $modelversion . ") \n");
		fwrite($handle, "# State: " . strtoupper($stateid) . "\n");  
		fwrite($handle, "# Climate Station: " . $climatestation . "\n");
		fwrite($handle, $numevents . " # The number of rain events\n");
		fwrite($handle, "0 # Breakpoint data? (0 for no, 1 for yes)\n");
		fwrite($handle, "#  id     day  month  year  Rain   Dur    Tp     Ip\n");
		fwrite($handle, "#                           (mm)   (h)\n");
	
		fwrite($handle, $storm_content);
		fclose($handle);
	}
	
	/**
	 * Builds the slope and normalized distance parameters for the input parameters file
	 *
	 */
	function createSlopeParameters($units,$slopelength,$slopeshape,$slopesteepness)
	{
		// convert OFE's length to metric units based on user selection
		if($units == 'english')
			$slopelength = $slopelength * 0.3048;
		$SL = "		SL	=	";
		$SX = "		SX	=	";
		switch ($slopeshape) {
			case 1: // "Uniform"
				$SL = $SL . $slopesteepness . "	,	" . $slopesteepness . "\n";
				$SX = $SX . "0.00	,	1.00 \n";
				break;
			case 2: // "Convex"
				$SL = $SL . "0.001	,	" . $slopesteepness * 2 . "\n";
				$SX = $SX . "0.00	,	1.00 \n";
				break;
			case 3: // "Concave"
				$SL = $SL . $slopesteepness * 2  . "	,	0.001\n";
				$SX = $SX . "0.00	,	1.00 \n";
				break;
			case 4: // "S-shaped"
				$SL = $SL . "0.001	,	" .  $slopesteepness * 2  . "	,	0.001\n";
				$SX = $SX . "0.00	,	0.50	,	1.00 \n";
				break;
		}
		
		return $SL . $SX;
	}
	

	/**
	 * Filter precipitation events based on Ke
	 *   NOTE: This version of the function will read the new Cligen output format 
	 *         (based on modificaitons by Mariano)
	 */
	function extractPrecipFromCligenOutput($scenarioname,$stateid,$climatestation,$Ke,$stormFileName)
	{
		$handle = fopen($stormFileName, "w");
		// change the directory to output cligen files
		chdir(CLIGEN_FOLDER . $stateid . '/300yr');
		$file = fopen(strtoupper($stateid) . "_" . $climatestation . "_300yr.out", "r") or exit("Unable to open Cligen output file!");

		//Output a line of the file until the end is reached
		$lineNumberAfterFilter = 1;

		$lineNumber = 0;
		while(!feof($file))
		{
			$line = fgets($file);
			
			// print data from non-blank lines
			if($line != '' and $lineNumber > 17)
			{	
			 	$linearray = preg_split('/\s+/', $line);

				$KeComparisonValue = $linearray[8] * ($linearray[5]/$linearray[6]);

				// include rain events only if Ke >=   ip    *   (  P  /  D  )
				if($Ke < $KeComparisonValue)
				{
					$values = str_pad($lineNumberAfterFilter,10," ",STR_PAD_BOTH) . str_pad($linearray[2], 6) . 
					str_pad($linearray[3],6) . str_pad($linearray[4],6) . str_pad($linearray[5],7) . 
					str_pad($linearray[6],7) . str_pad($linearray[7],7) . str_pad($linearray[8],4);		

					fwrite($handle,  $values  . PHP_EOL);
					
					$lineNumberAfterFilter = $lineNumberAfterFilter + 1;
				}
			 }
			$lineNumber = $lineNumber + 1;
		}
		fclose($file);
		fclose($handle);
		chdir(getcwd());
		
		// return the number of events after the Ke filter has been applied
		return $lineNumberAfterFilter - 1;
	}
}
?>