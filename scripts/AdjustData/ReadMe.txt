The zip file unzips to the following files:

ReadMe.txt          This file.
AdjustData.r        R program to run the program "Xogen" on sample data.
Xogen.r             R program to compute a fit to exogeneous factors
ObservedData.csv    Data for GMST and exogenous factors

In order to run the program, first unzip all these files into a single
working directory. Then alter the heading of the program "AdjustData.r"
so that the "setwd" command (set working directory) points to the directory
containing the program and data files. Then run the program "AdjustData.r"
To see the process is action, run it 1 line at a time.

After it runs, issuing the commands names(zout) and names(zback) will
list the quantities produced.

