The repository for manuscript entitled "Improving the Forecast Accuracy of Wind Power by Leveraging Multiple Hierarchical Structure". We implement a LightGBM wrapper for fabletools, which allows cross-sectional reconciliation for machine learning base forecasts. Base forecasts and cross-sectionally reconciled forecasts are produced using numbered R scripts. Crosstemporal reconciliation and also accuracy analysis is performed. The file "lgbm.R" implements the LightGBM fabetools model, allowing hyperparameters to be explicitly specified. Before LightGBM base forecasts can be produced, the “lgbm.R” file must be run to instantiate in the active R environment.
Only 4 .rds files are provided which have been preprocessed from the open source datasets:
Charlie Plumley. Kelmarsh wind farm data. February 2022. doi:10.5281/zenodo.5841834.
Charlie Plumley. Penmanshiel wind farm data. February 2022. doi:10.5281/zenodo.5946808.
To run the scripts, you must first manually compute the features on the dataset by running “1 – compute_features.R”, as the .RDS files with features included were too large to upload to GitHub.

For any questions or concerns, please send me an email at lucas.english@sydney.edu.au.
