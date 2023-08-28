# Analyzing-Illegal-Psychostimulant-Trafficking-Networks-Using-Noisy-and-Sparse-Data
Code accompanying the paper Analyzing Illegal Psychostimulant Trafficking Networks Using Noisy and Sparse Data

This GitHub repository contains a series of scripts, that collectively will reproduce the results of the paper. 

Overall there are 3 key steps in the paper workflow:
Data Scrubbing, optimal aggregation and aggregation periods statistics in R and Excel
Network construction including correlation analysis in R
Post analysis visualizations and analysis in GIS software, UCINET, Excel
Below we list the scripts and provide the manipulation details conducted outside R and SAS.

**********************************************************************************************************
Step 1: Reproducing Figure 1 and Table 1
Use: 
CocaineSimulated.csv and 
StatsAndfFiguresForPaper.R 
to reproduce the figure and table

**********************************************************************************************************
Step 2: Reproducing Figure 2
Use:
CocaineSimulated.csv and 
OutputForCorrelatonNetwork_Range_tau.R	
*********************
Step 2a - Preliminary data step
Open up CocaineSimulated.csv in Excel.
Create a pivot table with State as the row value, study month as the column value and price (or any other observational value) as the main variable. Change the aggregate to count.
Copy paste the pivot table as value into a new file
Write “state” in A1
Delete grand totals if you copied them
Save the file StateCountRange.csv
*********************
Step 2b, run the optimization
To run the optimization, use
StateCountRange.csv and
run_opt_range.R	
The script will produce data files that you can use to create Figure 2 as laid out in step 2c.
*********************
Step 2c, create Figure 2
We will use Excel and R to create Figure 2
In Excel, open the file stage_range_results.csv
Create a pivot table with tau as the row value, kappa as the column value and num_exceed as the the main variable.
Transform the resulting pivot table into a datafile with three columns: tau, kappa, num_exceed, see the PlotNumStatePeriod.csv provided as an example
Save the file as PlotNumStatePeriod.csv
In Excel again, continue to use stage_range_results.csv
Create a new column for each gamma value you seek to investigate (the minimum number of periods with at least kappa observations for each state-state pair)
Populate each new column with a binary indicator indicating whether or not the state-state pair exceeds you gamma value
Create a new pivot table, with tau as the row value, then circle through your binary indicators as the the main variable, make sure to set the value field to count
Copy the needed information to create table of the same format as DataForPlot.csv provided, a datafile with three columns: tau, count and kappa
Save the file as DataForPlot.csv
Run createPlots.R to create Figure 2.
**********************************************************************************************************
Step 3: Creating Files with Inferred links between states
*********************
Step 3a: Creating statistical summaries using the optimal aggregated data
Use:
CocaineSimulated
OutputForCorrelationNetworks_Range_tau.R 
Run the R file to create the inputfile(s) for step 3b
*********************
Step 3b: Creating files with inferred links
Use:
The output files from OutputForCorrelatonNetwork_Range_tau.R
thres_iter.R: 
concQAP.R
allStats.R
BestMats.R
QAP_comput-ation.R
The files do the following computation:
A. Compute conc_rate, correlation etc. with  iterations 1-8 of anecdotal data.
B. QAP test on Concor-dance_Rate
C. Compute conc_rate, correlation, Anecdotal links, Infe-rred links iterations 1-8.
D. find best conc_rate, correlation for each file (for itera-tion 8)
E. compute  conc_rate for each correlation threshold. 
**********************************************************************************************************
Step 4: Reproduce Figure 4
Use:
The output files from Step 3 (examples given)
StatsAndFiguresForPaper.R	
Runt he second half of StatsAndFiguresForPaper to create Figure 4.
**********************************************************************************************************
Step 5: Reproducing Figures 5,6, and 9
Use:
Output from step 3
GIS software
These figures were manually created using GIS software
**********************************************************************************************************
Step 6: Reproducing Tables 2,3,4
Use:
DataFiles from step 3
Access the UCINET software, to reproduce Tables 2,3, and 4.
**********************************************************************************************************
Step 7: Reproducing Figure 7
Use:
MethTimeVolumeFake.csv 
MethPricePotencyFake.csv	
MethStatsAndFiguresForPaper.R	
Reproducing Figure 2 in the supplement
Use:
DataFiles from step 3
Access the UCINET software, to reproduce Figure 2 in the supplement



















