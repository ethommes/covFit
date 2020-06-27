# covFit

Quick start instructions:


1) Open the notebook DEMOv2.Rmd in the directory covFit_parent\covFit.  R notebooks consist of "chunks" of R code that can be run by clicking the "Run current chunk" button (the green Play button) at the  top-right of each chunk.

2) Run the first chunk, the one that looks like this:
```{r}
setwd("../covFit")
source("main.R")
```
You'll almost certainly get a number of messages of the sort 
  Error in library(XYZ) : there is no package called ‘XYZ’

3) For each such message you receive, you'll have to install the corresponding library.  One of them is installed in a slightly nonstandard way, so we'll start with that one.  Run the following commands in the console:
  install.packages("devtools")
  library(devtools)
  install_github('cran/noncensus')
  
4) The remaining packages can be installed in the standard way, i.e. if the missing package is called XYZ, then
  install.packages("XYZ")
Keep installing missing packages, until you're able to execute the first chunk without getting any more error messages about missing packages.

5) Now, execute the other chunks in the worksheet in sequence.  Each one has some explanation, hopefully not too cryptic, about what it does.  Note that a lot of the chunks don't write output to the screen, but instead create .png or .jpg files in the directory covFit_parent\figs\DEMO or its subdirectories