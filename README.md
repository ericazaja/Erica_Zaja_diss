## Honours Dissertation Repository by Erica Zaja
### Ecological and Environmental Sciences with Management 
###  Last update: 04/04/22
*******

### Research Title: Shrubification in the Western Arctic and its Effects on the Porcupine Caribou Herd Habitat

#### Repository Structure

Repository includes: datasets, figures, R scripts, statistical output, Markdown structure, backup material, project organisation. 

- #### [datasets](https://github.com/ericazaja/dissertation/tree/main/datasets)

  - [PCH_Core_Range_2016](https://github.com/ericazaja/Erica_Zaja_diss/tree/main/datasets/PCH_Core_Range_2016): PCH core range data (Porcupine Caribou Management Board, 2016)

  - [berner_data](https://github.com/ericazaja/dissertation/tree/main/datasets/berner_data): Shrub map of North Slope of Alaska (Berner et al, 2021)

  - [ITEX_data](https://github.com/ericazaja/dissertation/tree/main/datasets/ITEX_data): plot-based vegetation cover data 

  - [phenology_data](https://github.com/ericazaja/dissertation/tree/main/datasets/phenology_data): ITEX phenology data (Prevéy et al, 2021)

  - [climate_data](): CHELSA temperature and precipitation data 

  - [anna_data](https://github.com/ericazaja/dissertation/tree/main/datasets/anna_data): data used for model RMarkdown dissertation - **NOT** actual data I need 

- #### [main_scripts](https://github.com/ericazaja/Erica_Zaja_diss/tree/main/scripts/main_scripts): 
  - [RQ1_mapping_EZ.R](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/main_scripts/RQ1_mapping_EZ.R): script with full shrub raster, full PCH range and cropped shrub map code
  - [RQ1_sampling_EZ.R](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/main_scripts/RQ1_sampling_EZ.R): script with focal study area ID and buffered random sampling method
  - [RQ1_models_EZ.R](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/main_scripts/RQ1_models_EZ.R): script with models to answer RQ1
  - [RQ2_extraction_EZ.R](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/main_scripts/RQ2_extraction_EZ.R): script with climate extraction code  
  - [RQ2_models_EZ.R](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/main_scripts/RQ2_models_EZ.R): script with models to answer RQ2
  - [RQ3_mariana_cleaning_EZ](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/main_scripts/RQ3_mariana_cleaning_EZ.R): script adapted from Mariana Garcia Criado's ITEX dataset data wranglig script. Modified to keep only functional groups I needed. 
  - [RQ3_vegcover_EZ.R](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/main_scripts/RQ3_vegcover_EZ.R): script with models for cover of functional groups over time in the ANWR
  - [RQ3_shrubcover_EZ.R](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/main_scripts/RQ3_shrubcover_EZ.R): script with models for cover of shrubs over time in the ANWR
  - [RQ4_wrangle_EZ.R](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/main_scripts/RQ4_wrangle_EZ.R): script with data wrangling for shrub phenology analysis 
  - [RQ4_models_EZ.R](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/main_scripts/RQ4_models_EZ.R): script with shrub phenology models
  
- #### [other_scripts](https://github.com/ericazaja/Erica_Zaja_diss/tree/main/scripts/other_scripts)
  - [RQ1_extras_EZ](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/other_scripts/RQ1_extras_EZ.R): NOT USED extra code/analyses for RQ1
  - [RQ2_extras_EZ](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/other_scripts/RQ2_extras_EZ.R): NOT USED extra code/analyses for RQ2
  - [RQ3_extras_EZ](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/other_scripts/RQ3_extras_EZ.R):  NOT USED extra code/analyses for RQ3
  - [RQ3_shrubcover_separate_EZ.R](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/other_scripts/RQ3_shrubcover_separate_EZ.R): script with separate models for each shrub genus
  - [RQ3_vegcover_separate_EZ.R](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/other_scripts/RQ3_vegcover_separate_EZ.R): script with separate models for each functional group
  - [RQ4_extras_EZ](https://github.com/ericazaja/Erica_Zaja_diss/blob/main/scripts/other_scripts/RQ4_extras_EZ.R):  NOT USED extra code/analyses for RQ4
  - other scripts including scraps and experiments
  
- #### [img](https://github.com/ericazaja/dissertation/tree/main/img):
  - figures produced 

- #### [sections](https://github.com/ericazaja/dissertation/tree/main/sections): 
  - separate Rmd sections of dissertation report
  
- #### [documents](https://github.com/ericazaja/dissertation/tree/main/documents): 
  - safety and ethics signed forms, title proposal, outline plan, budget form
  
- #### [proposals](https://github.com/ericazaja/dissertation/tree/main/proposals): 
  - one page outline, full thesis plan and proposed methods

#### Requirements
- `RStudio` version 1.2.5001 or greater
- packages `rgdal`, `sp`, `raster`, `ggplot2` 

#### My Learning Objectives
- Familiarising with GIS techniques
- Learning spatial analysis
- Working with large datasets and data wrangling
- Using data to answer ecological questions

#### Feedback Etiquette

- Please use either an issue or a pull request.
- Please use "###" and your initial before your feedback comments.
- Please feel free to comment on anything at all! Be harsh ;) 
- I'm happy to answer any questions you might have about my work.

### Acknowledgements
I thank the data providers, my supervisor Dr Isla Myers Smith, and Team Shrub members for their support. 
