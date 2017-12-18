# lazerhawk

[![Build Status](https://travis-ci.org/m-clark/lazerhawk.svg?branch=master)](https://travis-ci.org/m-clark/lazerhawk)
[![codecov](https://codecov.io/gh/m-clark/lazerhawk/branch/master/graph/badge.svg)](https://codecov.io/gh/m-clark/lazerhawk)

<img src="lh2_o.jpg" style="display:block; margin: 0 auto;" width='50%'>


## Package description

This package has a few miscellaneous functions useful to me, and which might be useful to others. It will be added to over time, and likely will have functions from all over the place as far as utility is concerned.  Nothing extraordinarily unique is here at present, more or less tweaks/variations of available R functions that do almost what I want, but not quite.  Also, I wanted to learn more about building packages, and this is my first foray into that world.  Very little testing of the functions has been done at this point, but the package itself would pass on CRAN, and testing will continue to expand.  Feel free to note issues if you use it.

I was listening to [Lazerhawk](http://lazerhawk.bandcamp.com/album/redline) at one point while creating it, hence the name, and which I can suggest is decent to listen to for programming, but to each their own in that regard.

## Installation

The usual github installation. **devtools** package required.

```{r}
devtools::install_github('m-clark/lazerhawk')
```

## Functions

### createCorr
A function to create a correlation matrix. Useful, for example, in setting up simulations.

### lowerTri, upperTri
Return a triangular matrix, with some options on what specifically is returned.

### describeAll
A summary function for mixed data types that provides the information I usually want.

### pairwise
Apply an arbitrary function to pairwise combinations of rows or columns.

### createAdjacency, createEdges
Create an adjacency matrix, or use one to create an edge list.

### theme_trueMinimal, theme_plotly
Clean up your ggplot or plotly visualizations.

### sum_NA, sum_NaN, sum_blank
Get counts of nothing.

### create_palette
Create a palette including complementary, triadic, tetradic, etc. colors.

### num_by
A quick summarise, possibly with dplyr::group_by, that provides things like mean, sd, etc.

### cat_by
A quick summarise for categorical variables, possibly with dplyr::group_by, that provides frequencies and percentages of catgories.


## Addins

### insertImgCenterAddin 
For Rmarkdown files, inserts `<img ...>` with centered options filled in.

### insertSpanAddin 
For Rmarkdown files, inserts `<span class=''>`.


## Release Notes

- 0.1.9 Extend num_by and rewrite describeAll
- 0.1.8 Added num_by summary function and fixed issue #3
- 0.1.7 Added create_palette and made updates to palettes
- 0.1.6 Removed corrheat to its own package, added sum of missingness
- 0.1.5 Added plotly theme, modified help
- 0.1.4 Updates to corrheat to use psych package
- 0.1.3 Addins for span, slide
- 0.1.2 Added clean themes for ggvis and ggplot
- 0.1.1 Added createAdjaceny and createEdges, and start of addins
- 0.1.0 Initial release

