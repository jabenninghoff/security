
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

[Visit our website!](https://www.security-differently.com)

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
- [Measuring Incident
  Duration](https://jabenninghoff.github.io/security/analysis/measuring-incidents.html)
  (2025-03-19): Analysis to determine thresholds of sample size to
  reliably detect changes in incident impact (duration) using Monte
  Carlo simulation.
- [Risk Value
  Analysis](https://jabenninghoff.github.io/security/analysis/risk-value.html)
  (2024-11-03): An exploration of the value of cybersecurity risk
  reduction.
- [Risk Quantification
  Demo](https://jabenninghoff.github.io/security/analysis/rq-demo.html)
  (2024-08-15): Risk Quantification demonstration for my SIRAcon 2024
  talk, “[UnFAIR: Simplifying and Expanding Technology Risk
  Quantification](https://www.information-safety.org/2024/08/29/siracon-2024/).”
- [Risk Quantification
  Prototype](https://jabenninghoff.github.io/security/analysis/rq-prototype.html)
  (2024-07-23): Prototype Risk Quantification tool for my SIRAcon 2024
  talk, “[UnFAIR: Simplifying and Expanding Technology Risk
  Quantification](https://www.information-safety.org/2024/08/29/siracon-2024/).”
- [Visualizing Risk
  (Draft)](https://jabenninghoff.github.io/security/analysis/visualizing-risk.html)
  (2024-12-06): An exploration of different approaches to communicating
  security breach risk informed by prior work.

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
