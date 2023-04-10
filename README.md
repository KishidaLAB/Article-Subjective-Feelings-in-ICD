# Article-Subjective-Feelings-in-ICD

This repository contains the scripts and data used in *"Subjective Feelings associated with Expectations and Rewards during Risky Decision-making in Impulse Control Disorder"*


## Data
Raw data for the risky decision-making task is located in the `Data` folder as `.csv` files. Filenames are titled based on the subject ID ( which indicates whether a subject has been classified as ICD / nonICD ) and the medication status of the visit ( on / off their dopaminergic medication ). 

Each file contains 2 columns:
* Time_ms: Timestamps of each event during the task in milliseconds
* Description: A description of each event (i.e. Keypresses, which options were displayed, rewards, ... )

## Scripts
Analysis scripts have been coded in R and Stan. The required packages and libraries have been specified in each script.

*The `projPath` variable in each script defines the location of the data and analysis files. This variable should be changed to the local location of this repository.*

**R scripts** have been numbered in the order they should be run: 
#### `1_Analysis`
1. `1_parseData.R` parses events from the csv files.
2. `2_fitHappinessModel.R` calls the `Happiness.stan` script and fits the happiness model to each group (ICD On / ICD Off / non-ICD On / non_ICD Off).
3. `3_CalculateRatings.R` uses each individual's happiness model parameters to calculate a rating for all trials.
4. `4_saveHappinessMdlParams.R` extracts and saves the happiness model parameters for each group for plotting and tables.
5. `5_fitChoiceModel.R` fits a logistic regression model to model participant's choice between the sure bet and gamble options.


**`Happiness.stan`** - contains the stan script used to fit the happiness model. This script has been modified from the hBayesDM package ([hBayesDM/rdt_happiness.stan at master · cran/hBayesDM · GitHub](https://github.com/cran/hBayesDM/blob/master/inst/stan_files/rdt_happiness.stan))
```
Citation:
Ahn, W.-Y., Haines, N., & Zhang, L. (2017) Revealing neuro-computational mechanisms 
of reinforcement learning and decision-making with the hBayesDM package. 
Computational Psychiatry, 1:1. https://doi.org/10.1162/CPSY_a_00002.
```

#### `2_Figures`
This folder contains scripts to reproduce figures made in R:
* Figure 2: Happiness Model Performance
* Figure 3: Posterior Distributions over group-level happiness model parameters
* Figure 4: Difference of means

#### `3_Tables`
This folder holds R scripts to generate tables in the article.
* Table 1: Gamble Choice Model Parameters
* Table 2: Happiness Model Group-Level Parameter Comparisons: ICD versus non-ICD
* Table 3: Happiness Model Group-Level Parameter Comparisons: On versus Off


* Supplemental Table 1: 
  * A) Demographic and Medication Information Per Patient - This table has been uploaded to the `Tables` folder
  * B) Demographic and Medication Information for the Non-ICD and ICD Groups
* Supplemental Table 2: Gambling rate in "Sure bet or Gamble" Task
* Supplemental Table 3: Happiness model individual-level parameter comparisons: ICD versus non-ICD
* Supplemental Table 4: Happiness model individual-level parameter comparisons: On versus Off medication
* Supplemental Table 5: Gambling rate in trials where gambling was not optimal
* Supplemental Table 6: Number of trials completed and Number of late trials for each visit

## Licensing

This code is being released with a permissive license. Please feel free to use or adapt the code under the terms of the license. If you make use of the code, we would appreciate that you cite the work.
