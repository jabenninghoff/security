
<!-- README.md is generated from README.Rmd. Please edit that file -->

# security

<!-- badges: start -->

[![R-CMD-check](https://github.com/jabenninghoff/security/workflows/R-CMD-check/badge.svg)](https://github.com/jabenninghoff/security/actions)
[![lint](https://github.com/jabenninghoff/security/workflows/lint/badge.svg)](https://github.com/jabenninghoff/security/actions)
<!-- badges: end -->

## Overview

**Security Differently:** A collection of notebooks for analyzing
security differently.

Feel free to use and/or fork this project!

## Notebooks

Notebooks in this package:

- [Measuring Changes in Breach
  Rates](https://jabenninghoff.github.io/security/analysis/breach-rates.html)
  (2024-06-05): A critical review of using breach frequency as a measure
  of security success, inspired by [Incident Metrics in SRE: Critically
  Evaluating MTTR and
  Friends](https://sre.google/resources/practices-and-processes/incident-metrics-in-sre/).
- [Constraints vs
  Performance](https://jabenninghoff.github.io/security/analysis/constraints.html)
  (2024-04-30): Visualizations exploring the use of constraints vs
  performance improvements in risk management.
- [Technology Risk
  Quantification](https://jabenninghoff.github.io/security/analysis/risk-quant.html)
  (2024-07-23): Prototype Risk Quantification tool for my SIRAcon 2024
  talk, “[UnFAIR: Simplifying and Expanding Technology Risk
  Quantification](https://web.cvent.com/event/7f49b0a6-bca9-46fd-8245-a2deb671efee/websitePage:23d1376e-2723-411a-910a-0edf87b03015?session=956b9176-13ad-4dac-af3c-e8ccb30bae8a&shareLink=true).”

## Installation

You can install the development version of security from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jabenninghoff/security")
```

Or using renv:

``` r
# install.packages("renv")
renv::install("jabenninghoff/security")
```

## Development

- Changelog: See “Changelog” or `NEWS.md`.
- Planned: [TODO](TODO.md)
