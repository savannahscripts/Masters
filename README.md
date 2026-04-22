# Masters
Scripts for Master's project



Script Order:

1. FM_LOAD.R

What I do: Load the FM dataset from access (by connecting to a SQLite database as I am operating on a mac), join tables 
   
2. FM_SPP_CLEAN.R

What I do: Clean species names, removing messy entries, NA values etc, remove freshwater species, upload and merge taxonomic information from WORMS database into FM dataset, remove elasmobranchs and all other non-teleosts
  
3. FM_LOC_CLEAN.R

What I do: Clean location names, Rewmove empty rows, Check years and find missing entry calues, clean method names, remove plankton methods (excluding larval data), remove NA columns

4. FM_METHODS.R

What I do: Investigate count metrics, remove records not classified to species level, remove methods with "unknown" count metric, investigate spread of count metric within methods and visualise, map geographic distribution of map

5. FM_MPAS_SPP.R

Species richness per site, unprotected sites, statistical significance

6. FM_GRID.R

Spatially grid
