Rental Deserts JCHS Working Paper, April 2024
Whitney Airgood-Obrycki, Magda Maaoui, & Sophia Wedeen

This project has 8 scripts for setting up the datasets and conducting the analysis.


Description of included scripts

DATA SETUP

1_GeographySetup_RentalDeserts2024: This code creates geographic variables. Each tract is categorized based on the location of its centroid. Variables specify if the tract falls in a metropolitan or micropolitan area, whether metropolitan tracts are city or suburban, and rural. A CBSA population rank variable is included for identifying the 100 most populous metros.
Produces file: tracts_cbsas_suburbs_2022.csv

2_DataOrganization_RentalDeserts2024: This code pulls 5-year Census variables of interest using the tidycensus package. The variables are merged into one tract-level dataset. The geography variables created in the first script are joined to create the full rental deserts dataset.
Produces file: RentalDeserts_dataset2022_041924.csv

3.1_ZoningFileSetup_TractInterpolation_RentalDeserts2024: This code is a geographic setup for merging the National Zoning and Land Use Database onto tracts. The NZLUD uses 2010 place/county subdivision and MSA geographies. The code assigns 2022 tract centroids to their corresponding 2010 place or county subdivision and 2010 CBSA to create a geographic crosswalk.
Produces file: tracts2022_places10_countysub10_cbsa10.csv

3.2_ZoningFileSetup_NZLUD_RentalDeserts2024: This code processes the National Zoning and Land Use Database files and merges the municipal and MSA files onto the geographic crosswalk created in script 3.1. The crosswalked file with joined NZLUD data is then merged onto the rental deserts dataset from script 2 to create a full rental deserts zoning dataset that is used for the second analysis.
Produces file: RentalDeserts_dataset2022_NZLUD_041924.csv

ANALYSIS

4.1

4.2

4.3

4.4
