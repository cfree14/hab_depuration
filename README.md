# Systematic review and meta-analysis of biotoxin depuration rates in marine seafood species

This the GitHub repository for the following paper:

* Free CM, Fang Y (2026) Marine biotoxin depuration rates: management applications, research priorities, and predictions for unstudied species.  _Harmful Algae_ [10.1016/j.hal.2026.103126](https://www.sciencedirect.com/science/article/pii/S1568988326000788).

Please contact Chris Free (cfree14@gmail.com) with questions about the paper or repository.

## Repository structure

The GitHub repository has the following general structure:

 * **data:** data used in the paper; each dataset has its own folder with the following subfolders:
   * **raw:** raw data
   * **processed:** processed data
   * R code to clean the raw data
 * **code:** code to conduct the phylogenetic regession analysis and to produce the figures in the paper
 * **tables:** tables for the paper
 * **figures:** figures for the paper
 * **output:** regression model objects and regression model predictions

The **data** folder contains subfolders for the following datasets:

* **fao:** FAO aquaculture and wild capture production statistics (coming later)
* **obis:** Observations of HAB species from the [OBIS database](https://obis.org/node/33dec23c-af65-4fb1-a437-79543c562ef0)
* **haedat:** Observations of toxigenic events resulting from HABs from the [HAEDAT database](https://haedat.iode.org/browseEvents.php) 
* **lit_review:** The database of published depuration rates and characteristics of their studies
* **extracted_data:** Data extracted from studies in which depuration rates and/or half lives are not directly reported
* **toxicities:** Data extracted from select studies documenting maximum toxicities observed in non-bivalve species
* **other:** Other miscellaneous data

Visitors may primarily be interested in the database of depuration rates or the predicted depuration rates. These are located in the following locations:

* **Database of depuration rates:** data/lit_review/processed/database.Rds (this folder includes other important meta-data on the final database)
* **Predictions of depuration rates:** output/bivalve_prediction_data.Rds

## Depuration forecast tool

We developed the following R Shiny web tool to help managers forecast depuration timelines and schedule biotoxin monitoring based on these forecasts: [https://emlab-ucsb.shinyapps.io/depuration_forecaster/](https://emlab-ucsb.shinyapps.io/depuration_forecaster/)
