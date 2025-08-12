# spatialCatalogueViewer: A 'Shiny' Tool to Create Interactive Catalogues of Geospatial Data

An R 'Shiny' application to seamlessly create an interactive a catalogue of geospatial data and deploy it online. The interface allows for map and table explorations of the catalogue. Items can be represented as points or areas.


[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/sebastien-plutniak/spatialCatalogueViewer/actions/workflows/r.yml/badge.svg)](https://github.com/sebastien-plutniak/spatialCatalogueViewer/actions/workflows/r.yml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16809720.svg)](https://doi.org/10.5281/zenodo.16809720)
[![SWH](https://archive.softwareheritage.org/badge/origin/https://github.com/sebastien-plutniak/spatialCatalogueViewer/)](https://archive.softwareheritage.org/browse/origin/?origin_url=https://github.com/sebastien-plutniak/spatialCatalogueViewer)
[![r-universe](https://sebastien-plutniak.r-universe.dev/badges/spatialCatalogueViewer)](https://sebastien-plutniak.r-universe.dev/ui#package:spatialCatalogueViewer)
[![license](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.r-project.org/Licenses/GPL-3)


# Contents

- [**Installation**](#installation)
- [**Use cases**](#use-cases)
- [**Community guidelines**](#community-guidelines)
  - [Reporting bugs](#reporting-bugs)
  - [Suggesting changes](#suggesting-changes)


# Installation

The package can be installed from CRAN with:

```r
install.packages("spatialCatalogueViewer")
```

The development version is available from *GitHub* and can be installed with:

```r
if ( ! requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes")
remotes::install_github("sebastien-plutniak/spatialCatalogueViewer")
```


# Use cases

`spatialCatalogueViewer` is used by:

*  [archeoViz Portal](https://analytics.huma-num.fr/archeoviz/home): a catalogue of archaeological datasets exposed with the `archeoViz` application [see R code](https://github.com/sebastien-plutniak/archeoviz-portal).
*  [open-archeOcsean](https://analytics.huma-num.fr/Sebastien.Plutniak/open-archeocsean): a catalogue of open-source datasets for the Pacific and Southeast Asia archaeology  [see R code](https://github.com/sebastien-plutniak/open-archeocsean).



# Community guidelines

## Reporting bugs

If you find a bug, please fill an [issue](https://github.com/sebastien-plutniak/spatialCatalogueViewer/issues) with all the details needed to reproduce it.

## Suggesting changes

Suggestions of changes to `archeofrag.gui` are very welcome. These requests may concern additional functions, changes to documentation, additional examples, new features, etc. 
They can be made by filling an [issue](https://github.com/sebastien-plutniak/spatialCatalogueViewer/issues) and, even better, using pull requests and the [GitHub Fork and Pull
model](https://help.github.com/articles/about-pull-requests).
