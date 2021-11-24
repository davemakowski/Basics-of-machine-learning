# Basics of machinelearning
Course, practical exercises, and project on machine learning.

The course includes four parts:
- Part 1: Introduction, objectives and main principles
- Part 2: Extensions of linear regression (PLS, penalized regression etc.)
- Part 3: Trees and forests (random forest)
- Part 4: Neural networks and deep learning (intro)

The practical exercises are on maize biomass forecasting. Two files are provided:
- A file (BiomassMais.txt) including (i) 680 biomass data for 40 different sites in France and for 17 years (1995 to 2011), mean temperatures during the first part of the growing season (day 1 to day 50), during the second part of the growing season (day 51 to day 100) and during the last part of the growing season (day 101 to day 150),noted T1, T2, T3, and average radiations during the same periods (RAD1, RAD2, RAD3).
- A file including R codes (MethodsBiomassMaize.R) predicting the final biomass of maize, noted B (g m-2), from the 6 input variables describing the climatic conditions during the growing season, under optimal water conditions. The R code fits several standard regression and machine learning tools and compares their accuracy using cross validation. 

Finally, two additional datasets are provided for organizing a project with students: TrainingDataSet_Maize.txt and TestDataSet_Maize.txt. These datasets include maize yield data and climate features for several French departments over several years. They can be used to organize a data challenge with a group of students. An example of project description is presented in the file MaizeChallenge_ML.pdf (In French).  

