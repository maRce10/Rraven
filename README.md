# Rraven

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/Rraven)](https://cran.r-project.org/package=Rraven)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/Rraven)](http://www.r-pkg.org/pkg/Rraven)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/Rraven)](https://cranlogs.r-pkg.org/badges/grand-total/Rraven)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)

The `Rraven` package is designed to facilitate the exchange of data between R and  [Raven sound analysis software](http://ravensoundsoftware.com) ([Cornell Lab of Ornithology](http://www.birds.cornell.edu)). [Raven](http://ravensoundsoftware.com) provides very  powerful tools for the analysis of (animal) sounds. R can simplify the automatization of complex routines of analyses. Furthermore, R packages as [warbleR](https://cran.r-project.org/package=warbleR), [seewave](https://cran.r-project.org/package=seewave) and [monitoR](https://cran.r-project.org/package=monitoR) (among others) provide additional methods of analysis, working as a perfect complement for those found in [Raven](http://ravensoundsoftware.com). Hence, bridging these applications can largely expand the bioacoustician's toolkit.

Currently, most analyses in [Raven](http://ravensoundsoftware.com) cannot be run in the background from a command terminal. Thus, most `Rraven` functions are design to simplify the exchange of data between the two programs, and in some cases, export files to  [Raven](http://ravensoundsoftware.com) for further analysis. 

Install/load the package from CRAN as follows:

```r

# From CRAN would be
#install.packages("Rraven")

#load package
library(Rraven)

```

To install the latest developmental version from [github](http://github.com/) you will need the R package [devtools](https://cran.r-project.org/package=devtools):

```r
# From CRAN would be
#install.packages("Rraven")

# From github
devtools::install_github("maRce10/Rraven")

#load package
library(Rraven)

```

The package vignette provides detailed examples for each function in `Rraven`, including both the R code as well as the additional steps in [Raven](http://ravensoundsoftware.com) required to fully accomplished the analyses. You can pull it up as follows:

```r

vignette("Rraven")


```

The animations explaining additional steps in [Raven](http://ravensoundsoftware.com) are shown in more detail on the [github](https://github.com/maRce10/Rraven) version of this vignette, which can be downloaded as follows (saves the file "Rraven.github.html" in your current working directory):


```r

download.file(url = "https://raw.githubusercontent.com/maRce10/Rraven/master/gifs/Rraven.hitgub.html", 
destfile = "Rraven.hitgub.html")

```
 &nbsp; 
 
The downloaded file can be opened by any internet browser.