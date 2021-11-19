# Buckets Dashboard
A Shiny app developed in R with reports for budgeting with the Buckets application (https://www.budgetwithbuckets.com).

Reads data directly from the .buckets file (sqlite format) and shows reports based on this.

The reports resemble those available in YNAB.

No extensive testing have been performed. Developed for personal use. Expect bugs.

# How to install
- Install R. This was developed in R version 4.1.2 (64-bit) running under Debian GNU/Linux 10 (buster).
- Install RStudio.
- Download and unzip this repository or clone it using git.
- Open RStudio and install the packages from the "library" calls in the file "global.R" using `install.packages("name_of_library")`.
- Edit "config.R". Especially important is to specify the path to the .buckets file.
- Run the app from RStudio by pressing "Run App" while in the "global.R", "ui.R" or "server.R" file.
Alternative is to set the working directory to the directory containing the .R files and then run:
`shiny::runApp(launch.browser = TRUE)`
