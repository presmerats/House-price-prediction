
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
		*   ![First factorial plane]("./Analysis Results/Unsupervised Analysis/PCA_variables_projection_firrst_factorial_plane.jpeg")
		*	txt explaining main results and next steps

	* 	for other analyses and models the same idea

---

## main.R
	# 1. loading
	load working dir

	load aux_functions.R
	source_scripts()   # <- our functions
	load_packages()

	# 2. read data
	# create datasets
	raw_continuous_vars_selection(data)


	# 3. unsupervised analysis


	# 4. linear models fitting



---

