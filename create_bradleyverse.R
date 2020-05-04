library(devtools)

devtools::install_github("mkearney/pkgverse")
library(pkgverse)

ip <- as.data.frame(installed.packages())

write.csv(ip,"bradleyverse_list.csv")

head(ip)

cat(paste(shQuote(ip$Package,type = "cmd"), collapse = ","))

#copy and paste manually

bradleyverse <- c("abind","acepack","acs","alphavantager","anytime","AsioHeaders","askpass","assertthat","athenar","attempt","AUC","AWR.Athena","aws.s3","aws.signature","babynames","backports","base","base64enc","BaylorEdPsych","BH","bibtex","bindr","bindrcpp","bit","bit64","bitops","blob","blogdown","bmp","bookdown","boot","brew","broom","BurStMisc","C3","Cairo","callr","car","carData","caret","caTools","cellranger","charlatan","checkmate","chron","class","classInt","cli","clipr","clisymbols","cluster","coda","codetools","colorspace","commonmark","compiler","corrplot","countrycode","covr","crayon","crosstalk","curl","data.table","data.world","dataMeta","datapasta","datasets","dbConnect","DBI","dbplyr","ddiff","deldir","desc","devtools","diagram","dichromat","digest","discogger","downloader","dplyr","DT","dtt","dummies","dwapi","e1071","edeR","educationdata","ellipsis","evaluate","extrafont","extrafontdb","fansi","farver","ff","FinCal","fivethirtyeight","flextable","forcats","foreach","forecast","foreign","formatR","Formula","fracdiff","fs","gdata","gdtools","generics","genius","geosphere","GGally","ggcal","ggforce","ggjoy","ggmap","ggplot2","ggplot2movies","ggraph","ggrepel","ggridges","ggstance","ggwordcloud","gh","git2r","glue","goftest","gower","gplots","graphics","graphlayouts","grDevices","grid","gridBase","gridExtra","gtable","gtools","gWidgets","haven","here","highlight","highr","Hmisc","hms","htmlTable","htmltools","htmlwidgets","httpuv","httr","hunspell","huxtable","igraph","imager","ineq","ini","ipred","isoband","ISOcodes","iterators","janeaustenr","janitor","jpeg","jsonlite","jtools","kableExtra","KernSmooth","knitr","labeling","LaCroixColoR","Lahman","later","lattice","latticeExtra","lava","lazyeval","leaflet","lifecycle","lme4","lmtest","log4r","lubridate","magick","magrittr","mailR","mapproj","maps","maptools","markdown","MASS","Matrix","MatrixModels","memoise","methods","metricsgraphics","mgcv","mime","miniUI","minqa","mnormt","ModelMetrics","modelr","mongolite","munsell","network","nlme","nloptr","NLP","nnet","noncensus","nord","numDeriv","ochRe","odbc","officer","opencpu","openssl","openxlsx","pacman","padr","pagedown","pander","parallel","pbkrtest","pdftools","PerformanceAnalytics","pillar","pkgbuild","pkgconfig","pkgdown","pkgload","pkgverse","plogr","plotrix","plyr","png","polyclip","praise","prettyunits","processx","prodlim","progress","promises","proto","protolite","ps","psych","purrr","quadprog","Quandl","quantmod","quantreg","R.cache","R.methodsS3","R.oo","R.utils","R6","RApiDatetime","rappdirs","raster","ratelimitr","rcmdcheck","RColorBrewer","Rcpp","RcppArmadillo","RcppEigen","RcppRoll","RCurl","readbitmap","readr","readxl","recipes","RefManageR","rematch","rematch2","remotes","reprex","reshape","reshape2","resume","reticulate","rex","rgdal","RgoogleMaps","riingo","rio","rJava","RJDBC","rjson","RJSONIO","rJython","rlang","rmarkdown","RMySQL","ROCR","roxygen2","rpart","rprojroot","rstudioapi","Rttf2pt1","rtweet","rversions","rvest","satconcordance","scales","scholar","SDMTools","selectr","sendmailR","servr","sessioninfo","sf","shape","shiny","shinyBS","shinydashboard","shinyFiles","shinyjs","shinythemes","showtext","showtextdb","skimr","slam","sna","snakecase","SnowballC","sourcetools","sp","SparseM","spatial","spatstat","spatstat.data","spatstat.utils","spData","splines","SQUAREM","ssh.utils","staplr","statnet.common","stats","stats4","stopwords","stringdist","stringi","stringr","survival","sys","sysfonts","syuzhet","tcltk","tensor","testthat","textshape","tibble","tidycensus","tidygraph","tidyquant","tidyr","tidyselect","tidytext","tidyverse","tiff","tigris","timeDate","timetk","timevis","tinytex","titanic","tm","tokenizers","tools","totalcensus","triebeard","truncnorm","tseries","TTR","tweenr","twitteR","units","urca","urltools","uroot","usethis","usmap","utf8","utils","uuid","vctrs","viridis","viridisLite","vitae","webshot","websocket","webutils","whisker","withr","wordcloud","xfun","xlsx","xlsxjars","XML","xml2","xmltools","xopen","xtable","xts","yaml","zeallot","zip","zipcode","zoo")


## vector of pkgs
tidyweb <- c("curl", "jsonlite", "httr", "xml2", "rvest", "purrr", "dplyr",
             "stringi", "gdns", "urltools", "iptools", "seleniumPipes", "webdriver",
             "HARtools", "xslt", "V8", "webreadr", "openssl", "splashr")

## create packages dir
dir.create("~/packages")

## create tidyweb pkgverse
pkgverse("tidyweb", tidyweb,
         keep = "~/packages",
         use = c("readme_rmd", "rstudio", "testthat", "mit_license", "git"),
         install_if = TRUE
)
