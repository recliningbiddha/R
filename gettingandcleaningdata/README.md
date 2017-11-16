coursera-getdata-006
====================

Peer review assignment of Coursera's "Getting and Cleaning Data" course.

Purpose
-------
Purpose of this repo is generating a tidy data from the source data.

Preparation
-----------
To achieve the above purpose, you need to do the following:

1. Prepare R environment and install `reshape2` package
2. Download the source data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
3. Unzip the source data into this directory
4. Run `run_analysis.R` script


Structure
---------
This repo consists of the following files:

- run_analysis.R : main R script to generate the tidy data.
- CodeBook.md : Documentation for the detail of the data.
- README.md : This readme file explaining the overview of this repo.

Internal of the Script
----------------------
1. Read feature name list
2. Read X, y and subject data for test set and training set, respectively
3. Give name to columns of the data frame of X using feature name list
4. Concatenate X, y and subject data for test set and training set, respectively
5. Concatenate test set and training set
6. Extract mean and standard deviation features
7. Overwrite activity id with activity name
8. Compute the means for each features, grouped by actibity and subject

