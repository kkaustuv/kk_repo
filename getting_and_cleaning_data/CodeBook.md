# Introduction

The script `run_analysis.R`performs all 5 steps described in the course project's definition.

* First, all the similar data is merged using the 'rbind()' function. Then using 'cbind()' functiona all datasets are merged to a single dataset
* Then all variables are assigned with descriptive naems. 'feature.txt' file is used to fetch all required variable names.
* As activity data is addressed with values 1:6, we take the activity names and IDs from `activity_labels.txt` and they are substituted in the dataset.
* On the whole dataset, those columns with vague column names are corrected.
* Finally, we generate a new dataset with all the average measures for each subject and activity type (30 subjects * 6 activities = 180 rows). The output file is called `averages_data.txt`, and uploaded to this repository.

# Variables

* `Activity_Test`, `Activity_Train`, `Subject_Test`, `Subject_Train`, `Feature_Test` and `Feature_Train` contain the data from the downloaded files.
* `m_activity`, `m_subject` and `m_feature` merge the previous datasets to further analysis.
* `m_data` stores combined data set with all columns
* `select_names` contains the correct names for the `m_data` dataset, which are applied to the column names stored in `m_mean_std_data`.
* A similar approach is taken with activity names through the `activities` variable.
* Finally, `m_avg_data` contains the relevant averages which is later stored in a `averagedata.txt` file. For aggregation of data `subset` funtion of `plyr' package is used.