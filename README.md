---
RESEARCH ARCHIVE
---

Thesis: A blended distance to define "people-like-me"

Student: Anaïs Fopma

Supervisors: Prof. Dr. Stef van Buuren, Dr. Gerko Vink & Mingyang Cai

---
# Introduction
This repository contains all necessary files to reproduce the results reported in the master's thesis: A blended distance to define "people-like-me".
The research consists of two simulation studies and an application to empirical data, investigating the properties of a blended distance measure. This measure is a weighted version of the predictive distance used in predictive mean matching (PMM), and the Mahalanobis distance. 

## Simulation study I
Simulation study I follows a full factorial design. The methods that are compared, are: the predictive distance, a ranked version of the blended metric with a blending factor of 1, 0.5, and 0, respectively, and a scaled version of the blended metric with a blending factor of 1, 0.5, and 0, respectively. The data-generating mechanisms are varied in their missingness mechanism, missingness proportion, distribution, and correlation: 
- The missingness mechanism is varied over two conditions: missing completely at random (MCAR) and missing at random (MAR). 
- The missingness proportion is varied over two conditions: 25% and 50%.
- The distribution is varied over two conditions: a normal distribution and a strongly skewed distribution.
- The correlation is varied over three conditions: a correlation of 0, 0.1, and 0.7.
This results in a total of 24 data-generating mechanisms over which the performance of each of the metrics was evaluated, primarily in terms of coverage and bias.  

## Simulation study II

## Application to empirical data 
The blended metric is also applied to data from the Sociaal Medisch Onderzoek Consultatiebureau Kinderen (SMOCK) study. The SMOCK database contains
the anonymised growth data of 1,933 children aged 0-15 months. In addition, the database contains covariates that influence growth, such as the sex, gestational age, birth weight, and height of the father and mother. The database is not publicly available. To request access to the data, please contact Anaïs Fopma: a.m.fopma@uu.nl. The use of these data for this study is approved by the Ethical Review Board of the Faculty of Social and Behavioural Sciences of Utrecht University. The approval is based on the documents sent by the researchers as requested in the form of the Ethics committee and filed under number 21-1906.

---
# Instructions for running the scripts
The repository contains the following files:

| Folders/Files            | Description   |
| -----------------        | ------------- |
|Simulation study I        |Folder containing all files for Simulation study I|
|/Workspaces               |Folder in which the workspaces of the simulation will be stored|
|/Simulation conditions I  |Folder containing the simulation conditions executed by '1. Execute_I.R'|
|/1. Execute_I.R           |Script to run Simulation study I|
|/2. Tables.R              |Script to create the tables with results|
|/3. Plots_coverage.R      |Script to create the plots of the coverage results|
|/4. Plots_bias.R          |Script to create the plots of the bias results|
|/Simulation study I.Rproj |R project of Simulation study I|
|/evaluate.function.R      |Function called by '1. Execute_I.R' and used to evaluate the simulation results|
|Requirements.md           |File containing the software and dependencies used, including version numbers. Note that all required packages are included in the scripts.|
|Thesis.pdf                |Thesis manuscript|

## Replication Simulation study I
To replicate the results of Simulation study I, follow these steps:
1. The blended metric is implemented in the mice.impute.blended function in a version of the [mice](https://cran.r-project.org/web/packages/mice/index.html) package that can be installed from [this branch](https://github.com/anaisfopma/mice/tree/development). To do so, run the following command in R: 
```
library(devtools)
install_github("anaisfopma/mice@development") 
```
2. Go to the 'Simulation study I' folder and open the 'Simulation study I.Rproj' file.
3. To run the simulation, open the '1. Execute_I.R' file and run all lines. The workspaces of each of the simulation conditions will be saved in the 'Workspaces' folder. 
4. To create the tables displayed in the thesis manuscript, open the '2. Tables.R' file and run all lines. The latex tables will be given in the output in the R console. 
5. To create the plots for the coverage results displayed in the thesis manuscript, open the '3. Plots_coverage.R' file and run all lines. 
6. To create the plots for the bias results displayed in the thesis manuscript, open the '4. Plots_bias.R' file and run all lines. 

---
# Permission and access
This archive is publicly available and can be used by anyone. For help or any issues with the archive, you can contact Anaïs Fopma: a.m.fopma@uu.nl.

