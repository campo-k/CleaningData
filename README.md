# Coursera Assingment
## Purpose
1. This is for Coursera Assigment
2. Lecture Name is "Getting and Cleaning Data Course Project" (week4)

## Configuration
1. run_analysis.R: This is script what create tidy data set.
2. codebook.txt: This is code book that explain created data set variable. (for this text file you need "feature_info.txt" what in downloaded zip file)
3. README.md

## How to run
1. Excute function "loadData()" (Before excute, download zipfile and unzip. and placed folder in working directory.)
2. loadData() function return list of data frame (Assign to variable.)
3. Excute function "dataMerge()"
4. dataMerge() function return tidy merge data(data frame) (Assign to variable.)
5. Excute function "avrBySubject()" (This will return average of each variable for each subject.)
6. Excute function "avrByActivity()" (This will return average of each variable for each activity.)
6. Excute function "avrBySubjectAndActivity()" (This will return average of each variable for each subject and each activity.)

