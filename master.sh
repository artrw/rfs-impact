#!/bin/sh
echo -e "\nHello world!\n"
#need to know where Rscript is housed on this computer
PATH=$PATH:/c/Program\ Files/R/R-3.6.1/bin
#need to know where a writable R package library is housed on this computer
#(can find in an R GUI by entering .libPaths() into the command line
export R_LIBS="C:/Users/artrw/Documents/R/win-library/3.6"
Rscript analysis.R
#the sed commands take stargazer output, replace textstrings "onestar", "twostar", "threestar", and "dollarsign"
#with "^{*}", "^{**}", "^{***}", and "$" respectively for appropriate LaTeX reading (jury rigged, I know...)
#also removes all formatting from the table so that only the body content is imported to the TeX file, where it
#actually formatted
sed 's/onestar/^{*}/g; s/twostar/^{**}/g; s/threestar/^{***}/g; s/dollarsign/$/g; 1,11d; /\\hline \\\\\[-1.8ex\]/,$d' tables/stationarity_raw.tex > tables/stationarity_edited.tex

sed 's/onestar/^{*}/g; s/twostar/^{**}/g; s/threestar/^{***}/g; s/dollarsign/$/g; 1,9d; /\\hline \\\\\[-1.8ex\]/,$d' tables/event_studies_raw.tex > tables/event_studies_edited.tex
sed -i '8 i\ {\\it \\textbf{Event 2}} & & & &\\\\' tables/event_studies_edited.tex
sed -i '16 i\ \\hline' tables/event_studies_edited.tex

sed 's/onestar/^{*}/g; s/twostar/^{**}/g; s/threestar/^{***}/g; s/dollarsign/$/g; 1,11d; /\\hline \\\\\[-1.8ex\]/,$d' tables/cointegration_raw.tex > tables/cointegration_edited.tex

D6Var_firms=`sed -n '10p' < tables/D6VAR_raw.tex`
sed 's/onestar/^{*}/g; s/twostar/^{**}/g; s/threestar/^{***}/g; s/dollarsign/$/g; 1,11d; /\\hline \\\\\[-1.8ex\]/,$d' tables/D6VAR_raw.tex > tables/D6VAR_edited.tex
sed -i "1 i\ $D6Var_firms\\\\\\\\ \\\hline" tables/D6VAR_edited.tex
sed -i '7 i\ \\hline' tables/D6VAR_edited.tex

D5Var_firms=`sed -n '10p' < tables/D5VAR_raw.tex`
sed 's/onestar/^{*}/g; s/twostar/^{**}/g; s/threestar/^{***}/g; s/dollarsign/$/g; 1,11d; /\\hline \\\\\[-1.8ex\]/,$d' tables/D5VAR_raw.tex > tables/D5VAR_edited.tex
sed -i "1 i\ $D5Var_firms\\\\\\\\ \\\hline" tables/D5VAR_edited.tex
sed -i '9 i\ \\hline' tables/D5VAR_edited.tex

D4Var_firms=`sed -n '10p' < tables/D4VAR_raw.tex`
sed 's/onestar/^{*}/g; s/twostar/^{**}/g; s/threestar/^{***}/g; s/dollarsign/$/g; 1,11d; /\\hline \\\\\[-1.8ex\]/,$d' tables/D4VAR_raw.tex > tables/D4VAR_edited.tex
sed -i "1 i\ $D4Var_firms\\\\\\\\ \\\hline" tables/D4VAR_edited.tex
sed -i '7 i\ \\hline' tables/D4VAR_edited.tex

D6Var6lags_firms=`sed -n '10p' < tables/D6VAR6lags_raw.tex`
sed 's/onestar/^{*}/g; s/twostar/^{**}/g; s/threestar/^{***}/g; s/dollarsign/$/g; 1,11d; /\\hline \\\\\[-1.8ex\]/,$d' tables/D6VAR6lags_raw.tex > tables/D6VAR6lags_edited.tex
sed -i "1 i\ $D6Var_firms\\\\\\\\ \\\hline" tables/D6VAR6lags_edited.tex
sed -i '9 i\ \\hline' tables/D6VAR6lags_edited.tex

D5Var6lags_firms=`sed -n '10p' < tables/D5VAR6lags_raw.tex`
sed 's/onestar/^{*}/g; s/twostar/^{**}/g; s/threestar/^{***}/g; s/dollarsign/$/g; 1,11d; /\\hline \\\\\[-1.8ex\]/,$d' tables/D5VAR6lags_raw.tex > tables/D5VAR6lags_edited.tex
sed -i "1 i\ $D5Var_firms\\\\\\\\ \\\hline" tables/D5VAR6lags_edited.tex
sed -i '9 i\ \\hline' tables/D5VAR6lags_edited.tex

D4Var6lags_firms=`sed -n '10p' < tables/D4VAR6lags_raw.tex`
sed 's/onestar/^{*}/g; s/twostar/^{**}/g; s/threestar/^{***}/g; s/dollarsign/$/g; 1,11d; /\\hline \\\\\[-1.8ex\]/,$d' tables/D4VAR6lags_raw.tex > tables/D4VAR6lags_edited.tex
sed -i "1 i\ $D4Var_firms\\\\\\\\ \\\hline" tables/D4VAR6lags_edited.tex
sed -i '9 i\ \\hline' tables/D4VAR6lags_edited.tex

D5VarRUS30000_firms=`sed -n '10p' < tables/D5VARRUS3000_raw.tex`
sed 's/onestar/^{*}/g; s/twostar/^{**}/g; s/threestar/^{***}/g; s/dollarsign/$/g; 1,11d; /\\hline \\\\\[-1.8ex\]/,$d' tables/D5VARRUS3000_raw.tex > tables/D5VARRUS3000_edited.tex
sed -i "1 i\ $D5Var_firms\\\\\\\\ \\\hline" tables/D5VARRUS3000_edited.tex
sed -i '9 i\ \\hline' tables/D5VARRUS3000_edited.tex

D5VarWTI_firms=`sed -n '10p' < tables/D5VARWTI_raw.tex`
sed 's/onestar/^{*}/g; s/twostar/^{**}/g; s/threestar/^{***}/g; s/dollarsign/$/g; 1,11d; /\\hline \\\\\[-1.8ex\]/,$d' tables/D5VARWTI_raw.tex > tables/D5VARWTI_edited.tex
sed -i "1 i\ $D5Var_firms\\\\\\\\ \\\hline" tables/D5VARWTI_edited.tex
sed -i '9 i\ \\hline' tables/D5VARWTI_edited.tex

pdflatex draft.tex
bibtex draft
pdflatex draft.tex