---
RESEARCH ARCHIVE
---

Thesis: A blended distance to define "people-like-me"

Student: Anaïs Fopma

Supervisors: Prof. Dr. Stef van Buuren, Dr. Gerko Vink & Mingyang Cai

---
# Introduction
This repository contains all necessary files to reproduce the results reported in the master's thesis: A blended distance to define "people-like-me".
The research consists of two simulation studies investigating the properties of a blended distance measure. This measure is a weighted version of the predictive distance used in predictive mean matching (PMM), and the Mahalanobis distance. 

## Simulation study I
Simulation study I follows a full factorial design. The methods that are compared, are: the predictive distance, a ranked version of the blended metric with a blending factor of 1, 0.5, and 0, respectively, and a scaled version of the blended metric with a blending factor of 1, 0.5, and 0, respectively. The data-generating mechanisms are varied in their missingness mechanism, missingness proportion, distribution, and correlation: 
- The missingness mechanism is varied over two conditions: missing completely at random (MCAR) and missing at random (MAR). 
- The missingness proportion is varied over two conditions: 25% and 50%.
- The distribution is varied over two conditions: a normal distribution and a strongly skewed distribution.
- The correlation is varied over three conditions: a correlation of 0, 0.1, and 0.7.  

This results in a total of 24 data-generating mechanisms over which the performance of each of the metrics is evaluated, primarily in terms of coverage, bias and proportion of explained variance.  

## Simulation study II
In the second simulation, the ranked version of the blended metric is evaluated with blending factors ranging from 0 to 1, with intervals of 0.1. The data-generating conditions are a skewed distribution and correlation of 0.7. The number of simulations is set to 10000. In every simulation, the outcome for a single random case is made incomplete and thereafter imputed 50 times. Performance is measured primarily in terms of accuracy, validity, and precision. 

## Ethics
The study is approved by the Ethical Review Board of the Faculty of Social and Behavioural Sciences of Utrecht University. The approval is based on the documents sent by the researchers as requested in the form of the Ethics committee and filed under number 21-1906. The approval is valid through 09 May 2022. The approval of the Ethical Review Board concerns ethical aspects, as well as data management and privacy issues (including the GDPR).


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
|/5. Plots_rsquared.R      |Script to create the plots of the R squared results|
|/Simulation study I.Rproj |R project of Simulation study I|
|/evaluate.function.R      |Function called by '1. Execute_I.R' and used to evaluate the simulation results|
|Simulation study II       |Folder containing all files for Simulation study II|
|/Workspaces               |Folder in which the workspaces of the simulation will be stored|
|/1. Execute_II.R          |Script to run Simulation study I|
|/2. Table&plots.R         |Script to create the table and plot of the results|
|/Simulation study II.Rproj|R project of Simulation study II|
|/evaluate.function.R      |Function called by '1. Execute_II.R' and used to evaluate the simulation results|
|Requirements.md           |File containing the software and dependencies used, including version numbers. Note that all required packages are included in the scripts.|
|Thesis.pdf                |Thesis manuscript|

## Reproduction Simulation study I
To reproduce the results of Simulation study I, follow these steps:
1. The blended metric is implemented in the mice.impute.blended function in an adapted version of the [mice](https://cran.r-project.org/web/packages/mice/index.html) package that can be installed from [this branch](https://github.com/anaisfopma/mice/tree/development). To do so, run the following command in R: 
```
library(devtools)
install_github("anaisfopma/mice@development") 
```
If this does not work, it is also possible to move the package to your default R library manually. Go to [the branch](https://github.com/anaisfopma/mice/tree/development), click on the Code button and choose Download ZIP. In your downloads folder, extract all files in the ZIP file, and rename the resulting folder 'mice'. Move the mice folder to your default R library (of which the location will probably look something like this: "C:\Users\Name\Documents\R\R-4.1.3\library").

2. Go to the 'Simulation study I' folder and open the 'Simulation study I.Rproj' file.
3. To run the simulation, open the '1. Execute_I.R' file and run all lines. The workspaces of each of the simulation conditions will be saved in the 'Workspaces' folder. 
4. To create the tables displayed in the thesis manuscript, open the '2. Tables.R' file and run all lines. The LaTex tables will be given in the output in the R console. 
5. To create the plots for the coverage results displayed in the thesis manuscript, open the '3. Plots_coverage.R' file and run all lines. If you have already run the code in '2. Tables.R', you can skip lines 14 to 111. 
6. To create the plots for the bias results displayed in the thesis manuscript, open the '4. Plots_bias.R' file and run all lines.  If you have already run the code in '2. Tables.R', you can skip lines 14 to 111. 
7. To create the plots for the R squared results displayed in the thesis manuscript, open the '5. Plots_rsquared.R' file and run all lines.

## Reproduction Simulation study II
To reproduce the results of Simulation study II, follow these steps:

1. Go to the 'Simulation study II' folder and open the 'Simulation study II.Rproj' file.
3. To run the simulation, open the '1. Execute_II.R' file and run all lines. The workspace will be saved in the 'Workspaces' folder. 
4. To create the table and plots displayed in the thesis manuscript, open the '2. Table&plots.R' file and run all lines. The LaTex table will be given in the output in the R console. 

---
# Permission and access
This archive is publicly available and can be used by anyone. For help or any issues with the archive, you can contact Anaïs Fopma: a.m.fopma@uu.nl.

