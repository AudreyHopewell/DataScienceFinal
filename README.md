To reproduce this project, download the data file "movies_metadata.csv" from Kaggle [here](https://www.kaggle.com/rounakbanik/the-movies-dataset#movies_metadata.csv).

Use the script "project_cleaning_variables.R" in "code" to import the data, clean it, subset it, and create new variables. This will result in two datasets, "movies.cat" and "movies.cluster" (corresponding to the two different target variables for prediction).

Run the script "project_prediction_intuitive.R" in "code" to generate results corresponding to the first analysis in the paper (classifying based on "intuitive" categorizations of movies).

Run the script "project_prediction_cluster.R" in "code" to generate results corresponding to the second analysis in the paper (classifying based on k-means clustering generated classes).

To recreate Figure 1, run the script "project_visualizations.R"
