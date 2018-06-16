
# Framework description


## Files
* main.R
* aux_functions.R
* 01_datapreparation_david.R
* linear_regression_fitting.R
* unsupervised_analysis.R
* Cross_validation.R
* trees_methods_fitting.R
* .. and more ..

**Discarded files? (this codes are copied to main.R)**

* execution_control.R
* main_datasets_generator.R
* main_experiments.R
* main_unsupervised_analysis.R

---

## Results storage

*	./Dataset
	*	preprocessed data,
		*	clean, outliers, missing, pca, log-transforms, subsets

*	./Analysis Results
	*	model_results.csv
		*	for each model and dataset training error, testing error
		*	written from inside the model function, or the CV function

	*   MLproject.xslx
		* overview of the models and datasets executed

	*	/Linear Model Fitting
		*	images or other evidence or results
		*	txt explaining main results and next steps

	*	/Unsupervised Analysis
		*	images or other evidence or results
		*   ![First factorial plane](https://bitbucket.org/upcfib18/mlproject/raw/0f4fc5f6752c985366a8d7f71a3c39e447bfd18d/Analysis%20Results/Unsupervised%20Analysis/PCA_variables_projection_first_factorial_plane.jpeg)
		*	txt explaining main results and next steps

	* 	for other analyses and models the same idea

---

## main.R
```python
rm(list = ls())
gc()


#### 1 - Set up environment ####

a = Sys.info()

david_wd <- "~/MIRI-Data Science/ML - Machine Learning/Project/mlproject/src"
asaf_wd <- ""
pau_wd <- "~/Projectes/mlproject/src"

if (a["user"] == "david") {
  wd <- david_wd
  } else if (a["user"] == "pau") {
   wd <- pau_wd
} else {
  wd <- asaf_wd
}
setwd(wd)

# source scripts

source("aux_functions.R")

source_scripts()

load_packages()





#### 2 - Read data ####

if (a[1] == "Linux"){
  # Linux reading file
  data <- read.csv(file="../Dataset/kc_house_data.csv", header=TRUE, sep=",")
  
} else {
  # windows reading file
  data <- read.csv(file="../Dataset/kc_house_data.csv", header=TRUE, sep=",")
}

#### 2.1 - Create datasets to be used ####

raw_continuous_dataset <- raw_continuous_vars_selection(data)
save(file = "../Dataset/raw_continuous_dataset.Rda", raw_continuous_dataset)
rm(raw_continuous_dataset)

#### 3 - Unsupervised analysis of the data ####

# test normality. 

# test-normality()

# perform PCA

pca_analysis()
pca_analysis_2()


# perform clustering





#### 4 - Linear Models fitting ####

load(file="../Dataset/raw_continuous_dataset.Rda")
linear_regression_fitting(raw_continuous_dataset, dataset_id = "raw_continuous_vars")


```


---

