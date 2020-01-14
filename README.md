# Industry Compliance Costs Under the Renewable Fuel Standard: Evidence from Compliance Credits

This repository contains .R, .sh, and .tex files for the paper "Industry Compliance Costs Under the Renewable Fuel Standard: Evidence from Compliance Credits" by Arthur R. Wardle (UC Berkeley) and Sherzod B. Akhundjanov (Utah State University). 

Missing from this repo are the data employed in the analysis, which are not public. If you have access to the data considered in the paper, here is how you can go about recreating our analysis:

1. You will need data on D6, D5, and D4 RINs from the 2015 compliance year. You will also need 2013 RINs in order to recreate every figure from the paper. These data will need to be stored in files titled "2015 RINs OPIS.csv" and "2013 RINs OPIS.csv" respectively and placed in the raw_data folder of the repo. The structure of the data should include a Date in column 1, followed by columns with headers "D6," "D5," and "D4," respectively. 

2. Likewise, the stock price, Russel 3000, and WTI data we use come from Finaeon GFD, whose data is not generally available to the general public. Recreating our analysis will require files titled "rus3000_finaeon_readable.csv", "wti_finaeon_readable.csv", and "stock_finaeon_readable.csv", each of which begin with a Date column. The first two should have only one other column labeled "RUS3000" and "WTI" respectively. The stock csv will have many other columns, each corresponding to one stock, and each column name will be that stock's ticker.

Getting the date extent of our data exactly correct is unnecessary; the .R file will cut off the extras before and after automatically.

In order to produce figures with the correct font, the analysis.R script will also need to be manually adjusted in order to tell the program where your local computer's copy of GhostScript (https://www.ghostscript.com/) is housed. GhostScript is only used to embed the correct font into R-generated figures. If you do not care about recreating pretty figures, this step is unnecessary. The .R code used to direct towards GhostScript is the first executed line of the file.

After all of the above is in place, running the master.sh file should execute all of the analysis and tex file compiling necessary and the .pdf of the paper should appear in the file. The stargazer output from analysis.R is also processed by master.sh (in an admittedly hackish way) in order to automate some of the formatting of the final tables. Keep this in mind if you attempt to change lag orders or other parts of the analysis that would affect the lengths of tables, because this will conflict with the post-processing conducted by master.sh.

Questions regarding anything in this project can be directed to the corresponding author, me

Arthur R. Wardle
PhD Student, UC Berkeley
arw@berkeley.edu
