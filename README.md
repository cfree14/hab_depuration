# Systematic review and meta-analysis of biotoxin depuration rates in marine seafood species

This the GitHub repository for a project seeking to predict biotoxin depuration rates for all low-trophic seafood species through the systematic review and meta-analysis of published depuration rates. The project is associated with the following paper in preparation

* Free CM, Fang Y (in prep) Biotoxin depuration rates from seafood species: a meta-analysis of analytic approaches and results. **Near submission** to _Harmful Algae_.

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

The **data** folder contains subfolders for the following datasets:

* **fao:** FAO aquaculture and wild capture production statistics (coming later)
* **obis:** Observations of HAB species from the [OBIS database](https://obis.org/node/33dec23c-af65-4fb1-a437-79543c562ef0)
* **haedat:** Observations of toxigenic events resulting from HABs from the [HAEDAT database](https://haedat.iode.org/browseEvents.php) 
* **lit_review:** The database of published depuration rates and characteristics of their studies
* **extracted_data:** Data extracted from studies in which depuration rates and/or half lives are not directly reported

Visitors may primarily be interested in the database of depuration rates or the predicted depuration rates. These are located in the following locations:

* **Database of depuration rates:** _file path coming soon_
* **Predictions of depuration rates:** _file path coming soon_
