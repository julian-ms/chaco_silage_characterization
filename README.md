# chaco_silage_characterization
Characterizing spatiotemporal dynamics of on-farm storage technologies in the Argentine Dry Chaco

Throughout the code the suffix "a" symbolizes the analysis of agricultural silage bags on cropland. The suffix "p" symbolizes the analysis of silage bags from a pastural context.

Variables, transformation and standardization
The data extraction results in five datasets. The first is the grid dataset, in which observations are not silage bags but grid cells and the variables are not distance-based.
Variable overview for dataset 1 (silage intensity, grid cells):
Paved: Length of paved road, Total meters of paved roads within grid cell
Unpaved: Length of unpaved road, Total meters of unpaved roads within grid cell
Rail: Length of railway,	Total meters of railways within grid cell
Silos: Number of silos,	Total number of silos within grid cell
Fridge: Number of fridges/slaughterhouses,	Total number of fridges/slaughterhouses within grid cell
Settlement: Number of settlements,	Total number of settlements within grid cell
Protected_area: Protected areas,	Total area (sqm) of protected areas within grid cell
Pop_mean: Average population density,	Population density in mean population per 85m by 85m raster pixel (as per data source); mean value within grid cell
Fl: Forest law,	4 discrete classes, where 0=no information, 1=red zone, 2=yellow zone and 3=green zone; majority class per grid cell
Total_count: Silage intensity,	Summed monthly counts of silage bags per grid cell

Data sets 2 and 3 consist of agricultural silage bags plus agricultural background points and pastural silage bags plus pastural background points. Data sets 4 and 5 are agricultural silage bags and pastural silage bags without background points, but they include the variable area. All continuous variables were standardized and some categorical variables were transformed as outlined in the tables X+Y. As part of the dataset development, correlation matrices of the continuous variables were constructed (consult the annex) for each dataset. The correlation between variables informed the decision which variables to include in the final datasets. If two variables were highly correlated (correlation coefficient over 0.5) only one was included.
Dist_paved: Distance to paved road,	Continuous distance to nearest-neighbour paved road (m)
Dist_unpaved: Distance to unpaved road,	Continuous distance to nearest-neighbour unpaved road (m)
Dist_rail: Distance to railway,	Continuous distance to nearest-neighbour railway (m)
Dist_silos: Distance to silos,	Continuous distance to nearest-neighbour unpaved silo (m); only in cropland datasets, as it proved to be insignificant in pastural analyses
Dist_fridge: Distance to fridges/slaughterhouses,	Continuous distance to nearest-neighbour unpaved fridge/slaughterhouse (m); only in pastural datasets
Dist_prot: Distance to protected areas,	Continuous distance to nearest-neighbour protected area (m)
Dist_settlement: Distance to settlements,	Continuous distance to nearest-neighbour settlement (m)
Pop_mean: Average population density,	Population density in mean population per 85m by 85m raster pixel (as per data source)
Fl: Forest law class,	4 discrete classes, where 0=no information, 1=red zone, 2=yellow zone and 3=green zone
Corn, soy: Crop type class,	Initially 16 discrete classes; was transformed into two binary variables: Corn (1 if crop type class was 0 or 3, 0 if not) and soy (1 if crop type class was 1 or 2, 0 if not); only in agricultural datasets
Silage: Silage presence,	Binary variable in datasets 2 and 3; 1 for silage bag observations and 0 for background points
Area: Area, Continuous area variable in sqm, due to the data source in steps of 100; only in datasets 4 and 5
