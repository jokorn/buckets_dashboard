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
- Some of these packages may fail to install as they are dependent on `libcurl` and `openssl`, which must be installed outside of R. Solutions differ depending on the OS, but try to install the following:

  For `libcurl`:

  - deb: libcurl4-openssl-dev (Debian, Ubuntu, etc)
  - rpm: libcurl-devel (Fedora, CentOS, RHEL)
  - csw: libcurl_dev (Solaris)

  For `openssl`:

  - deb: libssl-dev (Debian, Ubuntu, etc)
  - rpm: openssl-devel (Fedora, CentOS, RHEL)
  - csw: libssl_dev (Solaris)
  - brew: openssl@1.1 (Mac OSX)

- Edit "config.R". Especially important is to specify the path to the .buckets file, and to add "saving buckets" if you want to calculate your saving rate.
- Run the app. Multiple possibilities:
  1. From RStudio by pressing "Run App" while in the "global.R", "ui.R" or "server.R" file.
  2. Set the working directory to the directory containing the .R files and then run: `shiny::runApp(launch.browser = TRUE)`
  3. Via the terminal `Rscript -e "shiny::runApp('\''/path/to/the/appdir/'\'', launch.browser = TRUE)"`. This can easily be added to an `alias` or can be used together with a .Desktop file.
