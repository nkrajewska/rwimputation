# rwimputation

Random Walk algorithm imputation
[![R-CMD-check](https://github.com/nkrajewska/rwimputation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nkrajewska/rwimputation/actions/workflows/R-CMD-check.yaml)
R package for missing data imputation using the Random Walk algorithm,
optimized with OpenMP in C.

## Description

The `rwimputation` package provides solution for reconstructing missing
values (`NA`) in matrices and data frames. It support mixed-type data
(both numeric and categorical variables).

## Installation

You can install the development version of `rwimputation` directly from
GitHub using the `remotes` (or `devtools`) package:

\`\`\`R \# Install remotes if you haven’t already:
install.packages(“remotes”)

# Install the package from GitHub:

remotes::install_github(“nkrajewska/rwimputation”)
