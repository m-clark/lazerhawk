# lazerhawk

## Package description
This package has a few miscellaneous functions useful to me, and which might be useful to others. It will be added to over time, and likely will have functions all over the place.  I wanted to learn more about building packages, and this is my first foray into that world.  I was listening to some Lazerhawk at one point while creating it, hence the name.

Very little testing of the functions has been done at this point, but the package itself would pass on CRAN, and testing will continue to expand.  Feel free to note issues if you use it.

## Installation
The usual github installation. devtools package required.

```{r}
devtools::install_github('mclark--/lazerhawk')
```

## Functions
### corrheat
A copy of [d3heatmap](https://github.com/rstudio/d3heatmap) for correlation matrices, with a measurement focus.

### createCorr
A function to create a correlation matrix. Useful, for example, in setting up simulations.

### lowerTri, upperTri
Return a triangular matrix, with some options on what is returned.

### describeAll
A summary function for mixed data types that provides the information I usually want.

### pairwise
Apply an arbitrary function to pairwise combinations of rows or columns.
