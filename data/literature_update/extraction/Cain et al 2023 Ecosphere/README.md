# Native herbivore browsing alters plant physical and chemical traits in a eucalypt forest understory

Dataset contains six excel files containing raw data used for calculations of trait values for nine species in an open eucalypt understorey under two herbivory treatments. Data is also used to calculate community weighted means of those traits.

All data required to generate manuscript tables and figures as well as those in supplementary documents are included and detailed below.

Size: 83kb
Format: .csv (5 files)

## Description of the data and file structure

Each csv file is described below along with a description of any column headers.

**NutrientTraitData** --> contains raw carbon and nitrogen nutrient data for all species at each site.
Dimensions: 96 rows x 4 columns
Column headings:
Species = the species for which the data was measured.
Treatment = which treatment the species was under, open (accessible to Macropods) or exclosure (fenced off to Macropods).
C (%) = carbon as a percentage of dried plant material.
N (%) = nitrogen as a percentage of dried plant material.

**FieldMeasurements_TraitData** --> the raw trait data recorded for each individual plant in the field.
Dimensions: 459 rows x 14 columns
Column headings:
Site = designated site name.
Species = the species for which the data was measured.
PlantNo = the plant number at each site (within a maximum of 5 individuals of a species at each site).
Height = plant height in metres.
Width = plant width in metres.
CanopyDepth = canopy depth in metres.
Thickness = for columns 7-12 leaf thickness is included for each plant. Five leaves were measured for each plant and each individual recording is included in the row.
ThicknessAverage = the average leaf thickness for an individual plant in millimetres.
Date collected = the date the data was collected.
Treatment = which treatment the species was under, open (accessible to Macropods) or exclosure (fenced off to Macropods).

**FreshWeightCalibrationModel** --> raw fresh weight data recorded in the field and in the lab for analysis of time to measurement effects on fresh weight.
Dimensions: 651 rows x 4 columns
Column headings:
Site no. = designated site name.
Species name = the species for which the data was measured.
Fresh Weight = fresh weight of leaves
Time = a classification of time periods as either T1 or T2 where T1 is field measurements and T2 is post-fieldwork measurements.

**IndividualSpeciesCover** --> percentage cover of each individual species found at each site.
Dimensions: 13 rows x 41 columns
Column headings:
Site = designated site name.
Treatment = which treatment the species was under, open (accessible to Macropods) or exclosure (fenced off to Macropods).
Columns 3-41 = species names.

Data: The data in this spreadsheet contains either numerical values indicating species cover or a n/a value indicating that species was not detected at that site. 

**SLA_MC_TraitData** --> raw moisture content and specific leaf area measurements for each plant.
Dimensions: 456 rows x 5 columns
Column headings:
Site = designated site name.
Species = the species for which the data was measured.
Treatment = which treatment the species was under, open (accessible to Macropods) or exclosure (fenced off to Macropods).
SLA = specific leaf area of an individual.
MC = moisture content of an individual.

Manuscript figures and tables were generated using R version 4.1.0.

See published paper for further details.

## Sharing/Access Information

Not applicable.
