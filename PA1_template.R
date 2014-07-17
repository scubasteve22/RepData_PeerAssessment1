# Code to produce the knitr output
library(knitr)
setwd("./RepData_PeerAssessment1/")
knit("PA1_template.Rmd")    
knit2html("PA1_template.md", 
               output="PA1_template.html")

