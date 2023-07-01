# A-risk-scoring-model
## Code:

The following code underlies the analysis and findings of the paper "A risk scoring model of COVID-19 at hospital admission".


### Input data for scripts:

* _E1 - R\_INPUT.xlsx_: main source of data;
* _T1 - data.txt_: some preprocessing, mostly filters (result of R1 script);
* _T2 - data_ph.txt_: computation of hospital occupancy/prevalence (result of R2 script);
* _T3 - data_com.txt_: date columns were dropped;
* _T4 - data train.csv_ + _T4 - data test.txt_: division of data between train and test;
* **_E2 - train data to score.xlsx_**: _T4 - data train.csv_ is used to compute the score variables for gender, healthcare unit and comorbidities. The scores are then are used to build and test the model in R5, R6 and R7.

### R script flow:

The R scripts follows the sequence **R1- bdcomp.R**, **R2 - Prevalence.R**, **R3 -Evaluate the variables.R**, **R4 - split_train&test.R**, **R5 - train_build_model.R**, **R6 - test model.R**, **R7 - more plots.R**.

### R libraries:
* base;
* ggplot2;
* lubridate;
* MASS;
* plyr;
* ROCR;
* faraway;
* extrafont;
* reshape2;
