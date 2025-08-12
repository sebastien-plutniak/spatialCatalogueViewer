# spatialCatalogueViewer: A 'Shiny' Tool to Create Interactive Catalogues of Georeferenced Data

An R 'Shiny' application to seamlessly create an interactive and deploy online a catalogue of georeferenced data. The interface allows for map and table explorations of the catalogue. Items can be georeferenced as points or areas.


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

*  [archeoViz Portal](https://analytics.huma-num.fr/archeoviz/home): a catalogue of archaeological datasets exposed with the `archeoViz` application.
*  [open-archeOcsean](https://analytics.huma-num.fr/Sebastien.Plutniak/open-archeocsean): a catalogue of open-source datasets for the Pacific and Southeast Asia archaeology.



# Community guidelines

## Reporting bugs

If you find a bug, please fill an [issue](https://github.com/sebastien-plutniak/spatialCatalogueViewer/issues) with all the details needed to reproduce it.

## Suggesting changes

Suggestions of changes to `archeofrag.gui` are very welcome. These requests may concern additional functions, changes to documentation, additional examples, new features, etc. 
They can be made by filling an [issue](https://github.com/sebastien-plutniak/spatialCatalogueViewer/issues) and, even better, using pull requests and the [GitHub Fork and Pull
model](https://help.github.com/articles/about-pull-requests).
