# Buckets Dashboard
A Shiny app developed in R with reports for budgeting with the Buckets application (https://www.budgetwithbuckets.com).

Reads data directly from the .buckets file (sqlite format) and shows reports based on this.

The reports resemble those available in YNAB.

No extensive testing have been performed. Developed for personal use. Expect bugs.

# How to install
- Install R. This was developed in R version 4.1.2 (64-bit) running under Debian GNU/Linux 10 (buster) and with Buckets Beta v0.63.2.
- Install RStudio.
- Download and unzip this repository or clone it using git.
- Open RStudio and install the packages from the "library" calls in the file "global.R":
```r
install.packages(c("tidyverse",
                   "DBI",
                   "RSQLite",
                   "shiny",
                   "shinyWidgets",
                   "shinyjs",
                   "DT",
                   "janitor",
                   "plotly"))
```
- Edit "config.R". Especially important is to specify the path to the .buckets file.
- Run the app. Multiple possibilities:
  1. From RStudio by pressing "Run App" while in the "global.R", "ui.R" or "server.R" file.
  2. Set the working directory to the directory containing the .R files and then run: `shiny::runApp(launch.browser = TRUE)`
  3. Via the terminal `Rscript -e "shiny::runApp('\''/path/to/the/appdir/'\'', launch.browser = TRUE)"`. This can easily be added to an `alias` or can be used together with a .Desktop file.
