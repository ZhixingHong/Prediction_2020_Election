# Prediction_2020_Election

This repo contains the data and R code for predicting the 2020 US presidential election. It was created by Zhixing Hong, Lingyue Kong, Jinyu Luo. The aiming for this is to forming a report that presents the statistical model we created to forecast the election results. As some data is not allowed to share in public , we documented how to get them below. The section of the repo are: inputs, outputs, and scripts.


Inputs is the folder where the changed data stored. We use two datasets in the project:

- [survey data: download from https://www.voterstudygroup.org/publication/nationscape-data-set ]
- [census data: download from https://usa.ipums.org/usa/index.shtml]

Outputs is the folder where the modified data, report and other supporting file stored.

- census_cell.csv (cleaned data file used for post-stratification)
- census_data.csv (cleaned data file for census)
- survey_data.csv (cleaned data file for survey)
- Towards a forecasing of American Presidential Election in 2020.rmd

Scripts contains R scripts that takes inputs and produced the outputs.
They are:
- 01_cleaning_survey.R
- 01_cleaning_census.R
