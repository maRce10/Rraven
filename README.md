# Rraven

The `Rraven` package is designed to facilitate the exchange of data between R and  [Raven sound analysis software](http://www.birds.cornell.edu/brp/raven/RavenOverview.html) ([Cornell Lab of Ornithology](http://www.birds.cornell.edu)). [Raven](http://www.birds.cornell.edu/brp/raven/RavenOverview.html) provides very  powerful tools for the analysis of (animal) sounds. R can simplify the automatization of complex routines of analyses. Furthermore, R packages as [warbleR](https://cran.r-project.org/package=warbleR), [seewave](https://cran.r-project.org/package=seewave) and [monitoR](https://cran.r-project.org/package=monitoR) (among others) provide additional methods of analysis, working as a perfect complement for those found in [Raven](http://www.birds.cornell.edu/brp/raven/RavenOverview.html). Hence, bridging these applications can largely expand the bioacoustician's toolkit.

Currently, most analyses in [Raven](http://www.birds.cornell.edu/brp/raven/RavenOverview.html) cannot be run in the background from a command terminal. Thus, most `Rraven` functions are design to simplify the exchange of data between the two programs, and in some cases, export files to  [Raven](http://www.birds.cornell.edu/brp/raven/RavenOverview.html) for further analysis. 

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

The package vignette provides detailed examples for each function in `Rraven`, including both the R code as well as the additional steps in [Raven](http://www.birds.cornell.edu/brp/raven/RavenOverview.html) required to fully accomplished the analyses. You can pull it up as follows:

```r

vignette("Rraven")


```

Note that the vignette avaialble on [github](http://github.com/) (which can be downloaded from here) has more detailed animations of the [Raven](http://www.birds.cornell.edu/brp/raven/RavenOverview.html) steps.

