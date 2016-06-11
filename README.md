This is the source code for a WC data set explorer app which runs on R and Shiny. The data is a subset of WC data from ClaimPATH. The data is saved in a SQLite database.

To run it locally, you'll need to install the latest versions of [ggvis](http://ggvis.rstudio.com), [Shiny](http://shiny.rstudio.com), and [dplyr](https://github.com/hadley/dplyr), as well as [RSQLite](http://cran.r-project.org/web/packages/RSQLite/index.html).

```r
install.packages(c('shiny', 'ggvis', 'dplyr', 'RSQLite'))
```
Import tab delimeted text file to RSQLite:
'''r
library(sqldf)

dbcon <- dbConnect(SQLite(), dbname = "gbwc_fin.sqlite")

dbWriteTable(conn=dbcon,name="gbwc_financial", value = "wc_financial.txt",row.names=FALSE,header = TRUE, sep = "\t")

dbRemoveTable(conn=dbcon,name="gbwc_financial")

dbWriteTable(conn=dbcon,name="gbwc_financial", value = "wc_financial.txt",row.names=FALSE,header = TRUE, sep = "\t")

dbDisconnect(dbcon)
'''

You may need to restart R to make sure the newly-installed packages work properly.

After all these packages are installed, you can run this app by entering the directory, and then running the following in R:

```s
 library(shiny)
 apppath = "C:\\Users\\u041018\\Documents\\GitHub\\wc_financial"
 shiny::runApp(appDir = apppath,display.mode = "showcase")
```

read in tab dlm text file use

'''s
dbWriteTable(conn=dbcon,name="gbwc_financial", value = "wc_financial.txt",row.names=FALSE,header = TRUE, sep = "\t")
'''