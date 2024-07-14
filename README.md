# Buckets Dashboard

## Supported versions of Budget With Buckets

Starting from `commit 75e8e44`, the Buckets Dashboard is being developed for Buckets Beta v0.76.0. 
If you are using an older version of Buckets and you experience problems with backwards compatibility, please revert to a commit prior to `commit 75e8e44`. 
Some of the new functions in the Buckets Dashboard will only be available when using a version of Buckets that includes the `Payee` function.

## Introduction

A Shiny app developed in R with reports for budgeting with the Buckets application (https://www.budgetwithbuckets.com).

Reads data directly from the .buckets file (sqlite format) and shows reports based on this.

The reports resemble those available in YNAB.

No extensive testing have been performed. Developed for personal use. Expect bugs (and kindly report bugs in GitHub when you do experience them).

## How to install
- Install R. This was developed in R version 4.4.1 (64-bit) running under Debian GNU/Linux 12 (bookworm) and with Buckets Beta v0.76.0.
- Install RStudio.
- Download and unzip this repository or clone it using git.
- Open RStudio and install the packages from the "library" calls in the file "global.R":
```r
install.packages(c("tidyverse",
                   "lubridate",
                   "DBI",
                   "RSQLite",
                   "dplyr",
                   "shiny",
                   "shinyWidgets",
                   "shinyjs",
                   "DT",
                   "janitor",
                   "plotly",
                   "checkmate",
                   "shinyvalidate"))
```
- Some of these packages may fail to install as they are dependent on external libraries. Please look trough the error messages in RStudio in case the installation of some of the packages fail as there are often suggestions on how to solve the issue. The below external libraries must be installed outside of R. This list can change as R packages are updated. Solutions differ depending on the OS, but try to install the following:

  For `libcurl`:

  - deb: libcurl4-openssl-dev (Debian, Ubuntu, etc)
  - rpm: libcurl-devel (Fedora, CentOS, RHEL)
  - csw: libcurl_dev (Solaris)

  For `openssl`:

  - deb: libssl-dev (Debian, Ubuntu, etc)
  - rpm: openssl-devel (Fedora, CentOS, RHEL)
  - csw: libssl_dev (Solaris)
  - brew: openssl@1.1 (Mac OSX)

  For `fontconfig freetype2`:
  
  - deb: libfontconfig1-dev (Debian, Ubuntu, etc)
  - rpm: fontconfig-devel (Fedora, EPEL)
  - csw: fontconfig_dev (Solaris)
  - brew: freetype (OSX)
  
  For `harfbuzz freetype2 fribidi`:
  
  - deb: libharfbuzz-dev libfribidi-dev (Debian, Ubuntu, etc)
  - rpm: harfbuzz-devel fribidi-devel (Fedora, EPEL)
  - csw: libharfbuzz_dev libfribidi_dev (Solaris)
  - brew: harfbuzz fribidi (OSX)
  
  For `freetype2 libpng libtiff-4 libjpeg`:
  
  - deb: libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev (Debian, Ubuntu, etc)
  - rpm: freetype-devel libpng-devel libtiff-devel libjpeg-turbo-devel (Fedora, CentOS, RHEL)
  - csw: libfreetype_dev libpng16_dev libtiff_dev libjpeg_dev (Solaris)

- Edit "config.R". Especially important is to specify the path to the .buckets file, and to add "saving buckets" if you want to calculate your saving rate.
- Run the app. Multiple possibilities:
  1. From RStudio by pressing "Run App" while in the "global.R", "ui.R" or "server.R" file.
  2. Set the working directory to the directory containing the .R files and then run: `shiny::runApp(launch.browser = TRUE)`
  3. Via the terminal `Rscript -e "shiny::runApp('\''/path/to/the/appdir/'\'', launch.browser = TRUE)"`. This can easily be added to an `alias` or can be used together with a .Desktop file.
