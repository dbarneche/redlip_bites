# Dataset for fish bite rates
## data/bites_data.csv  
### Variables descriptions 
*spp:* Species name following the taxonomy provided in the online [Catalog of Fishes](http://researcharchive.calacademy.org/research/ichthyology/catalog/fishcatmain.asp);
*local:* Locality where the individual was sampled;
*site:* Site inside the locality, where the individual was sampled;
*latitude:* Latitude for the sampling site in decimals;
*longitude:* Longitude for the sampling site in decimals;
*day:* The day when sampling occurred;
*month:* The month when sampling occurred;
*year:* The year when sampling occurred;
*sampler:* Researcher that recorded the individual's bite rates in the field;
*temperature:* Temperature in the moment of bite rates sampling in °C;
*start:* Time of the day in which the entire sampling was started;
*end:* Time of the day in which the entire sampling was finished;
*depth:* Which depth the focus fish individual was recorded biting the substrate;
*ind:* Number of the individual;
*ont:* Ontogenetic stage of a fish individual identified by its coloration (Mendes 2007);
*size:* Total length in centimetres;
*obs_time:* Total time of observation for each individual in the field;
*bites_original:* Number of bites in the substratum considering the time defined in the column obs_time;
*bites_3min:* Number of bites in the substratum for 3 minutes;
*bites_5min:* Number of bites in the substratum for 5 minutes;
*substratum:* Target substratum.

## data/diet_data.csv  
### Variables descriptions 
*spp:* Species name following the taxonomy provided in the online [Catalog of Fishes](http://researcharchive.calacademy.org/research/ichthyology/catalog/fishcatmain.asp);
*local:* Locality where the individual was sampled;
*site:* Site inside the locality, where the individual was sampled;
*ind:* Number of the individual;
*latitude:* Latitude for the sampling site in decimals;
*longitude:* Longitude for the sampling site in decimals;
*sampler:* Researcher that recorded the individual's bite rates in the field;
*collaborator:* Researcher responsible for the diet analyses;
*fixation:* Chemical solution used for fixing food items;
*month:* The month when sampling occurred;
*year:* The year when sampling occurred;
*TL_mm:* Total length in millimetres;
*SL_mm:* Standard length in millimetres;
*weight_g:* Individual weight in grams;
*intestine_mm:* Intestine length in millimetres;
*QI:* Intestinal Coefficient (TL / intestine);
*mean_temp:* Mean water temperature obtained from bites_data;
*mouth_volume:* Mouth volume, values in cubic millimetres;
*fullness:* Visual estimate of how much the gut was full with food, 1=(0-25% full), 2=(25,1-50%), 3=(50,1-75%), 4=(75,1-100%);
*ni_algae:* Feeding item, non-identified algae
*bivalve:* Feeding item, class Bivalvia, values in cubic millimetres;
*ceramiales:* Feeding item, order Ceramiales, values in cubic millimetres;
*cladophorales:* Feeding item, order Cladophorales, values in cubic millimetres;
*copepoda:* Feeding item, subclass Copepoda, values in cubic millimetres;
*corallinales:* Feeding item, order Corallinales, values in cubic millimetres;
*dictyotales:* Feeding item, order Dictyotales, values in cubic millimetres;
*ectocarpales:* Feeding item, order Ectocarpales, values in cubic millimetres;
*eggs:* Feeding item, values in cubic millimetres;
*fish_scale:* Feeding item, values in cubic millimetres;
*gastropoda:* Feeding item, class Gastropoda, values in cubic millimetres;
*gelidiales:* Feeding item, class Gelidiales, values in cubic millimetres;
*hexacorallia:* Feeding item, subclass Hexacorallia, values in cubic millimetres;
*insect:* Feeding item, class Insecta, values in cubic millimetres;
*sediment:* Feeding item, inorganic material, values in cubic millimetres;
*polychaeta:* Feeding item, class Polychaeta, values in cubic millimetres;
*ulvales:* Feeding item, order Ulvales, values in cubic millimetres;
*plastic:* plastic found in the gut, values in cubic millimetres;
*organic_detritus:* Feeding item, organic detritus values in cubic millimetres;
*total:* Sum of the feeding items volume for the individual.
