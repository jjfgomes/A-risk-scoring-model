# A-risk-scoring-model
## Code:

The following code underlies the analysis and findings of the paper "A risk scoring model of COVID-19 at hospital admission".

In terms of research flow, the first R script assumes and takes as input the precomputed variable score versions of the categorical variables (gender, clinical diagnoses and healthcare unit).

### Input data for scripts:

* _train_data.txt_: train data with original variables (each line corresponds to a single comorbidity);
* _test.txt_: test data with original variables (each line corresponds to a single comorbidity);
* _score_dc_af.txt_: table matching each comorbidity or clinical diagnose with a numerical score;
* _score_gen.txt_: table matching each gender with a numerical score;
* _score_hosp.txt_: table matching each healthcare unit with a numerical score;

### R script flow:

The R scripts follows the sequence **01_train_build_model.R**, **02_test_performance.R**, **03_plots.R**.

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
