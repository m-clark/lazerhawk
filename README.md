lazerhawk <img src="man/img/lh_hex.png" align="right" width = 360/>
===================================================================

[![Build Status](https://travis-ci.org/m-clark/lazerhawk.svg?branch=master)](https://travis-ci.org/m-clark/lazerhawk) [![codecov](https://codecov.io/gh/m-clark/lazerhawk/branch/master/graph/badge.svg)](https://codecov.io/gh/m-clark/lazerhawk)

<!-- <img src="man/img/lh_hex.png" style="display:block; margin: 0 auto;" width='50%'> -->
Package description
-------------------

This package has a few miscellaneous functions useful to me, and which might be useful to others. It will be added to over time, and likely will have functions from all over the place as far as utility is concerned. Nothing extraordinarily unique is here at present, more or less tweaks/variations of available R functions that do almost what I want, but not quite. Also, I wanted to learn more about building packages, and this was my first foray into that world. The package itself is not on CRAN but would pass CRAN checks. Feel free to note issues if you use it.

I was listening to [Lazerhawk](http://lazerhawk.bandcamp.com/album/redline) at one point while creating it, hence the name, and which I can suggest is decent to listen to for programming, but to each their own in that regard.

Installation
------------

The usual GitHub installation. **devtools** package required.

``` r
devtools::install_github('m-clark/lazerhawk')
```

Functions
---------

### brmsSummaryTable

A function I use when creating reports for other people that involve models using the [brms](https://github.com/paul-buerkner/brms) package.

### cat\_by

A quick summarize for categorical variables, possibly with dplyr::group\_by, that provides frequencies and percentages of categories.

### combn\_2\_col

Takes a column with multiple entries per cell and creates indicator columns of all possible combinations of the cell values up to m combinations.

### createAdjacency, createEdges

Create an adjacency matrix, or use one to create an edge list.

### createCorr

A function to create a correlation matrix. Useful, for example, in setting up simulations.

### create\_palette

Create a palette including complementary, triadic, tetradic, etc. colors.

### create\_prediction\_data

Straightforward way to quickly create data to make model predictions.

### describe\_all

A summary function for mixed data types that provides the information I usually want.

### extract\_nlme\_variances

A very specific function for extracting heterogenous variances from nlme output.

### lowerTri, upperTri

Return a triangular matrix, with some options on what specifically is returned.

### num\_by

A quick summarize, possibly with dplyr::group\_by, that provides things like mean, sd, etc.

### num\_summary

A little better than summary.

### onehot

A function for one-hot encoding with a few helpful options.

### pairwise

Apply an arbitrary function to pairwise combinations of rows or columns.

### sum\_NA, sum\_NaN, sum\_blank

Understand your nothingness.

### theme\_trueMinimal, theme\_plotly

Clean up your ggplot or plotly visualizations.

Addins
------

### insertImgCenterAddin

For R Markdown files, inserts `<img ...>` with centered options filled in.

### insertSpanAddin

For R Markdown files, inserts `<span class=''>`.

Release Notes
-------------

-   0.2.1 Add create\_prediction\_data
-   0.2.0 Add combn\_2\_col, onehot, and extract\_nlme\_variances
-   0.1.9 Extend num\_by and rewrite describeAll
-   0.1.8 Added num\_by summary function and fixed issue \#3
-   0.1.7 Added create\_palette and made updates to palettes
-   0.1.6 Removed corrheat to its own package, added sum of missingness
-   0.1.5 Added plotly theme, modified help
-   0.1.4 Updates to corrheat to use psych package
-   0.1.3 Addins for span, slide
-   0.1.2 Added clean themes for ggvis and ggplot
-   0.1.1 Added createAdjacency and createEdges, and start of addins
-   0.1.0 Initial release
