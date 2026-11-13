# Benchmarking

The [CodSpeed CLI](https://github.com/CodSpeedHQ/codspeed) measures two deterministic
parametric workloads through its generic executable harness. Each named command
runs 25 package calls and checks the returned statistics and expression column.
Measurements include R startup and dependency loading, so they describe these
complete workloads. Native profiles may show R interpreter frames rather than
individual R functions.

## Run locally

Install the checkout and run each workload from the repository root:

```sh
R CMD INSTALL .
Rscript --vanilla benchmarks/run.R two-sample-parametric
Rscript --vanilla benchmarks/run.R correlation-parametric
```

These commands check execution and output without collecting CodSpeed measurements.
On a supported Linux machine with the CodSpeed CLI installed, use
`codspeed run --mode simulation` to measure the targets in `codspeed.yml`.

## CI and comparisons

- The `Benchmarks` workflow uses Ubuntu 24.04, R 4.6.1, and one thread for BLAS
  and OpenMP. Dependencies are resolved from the configured package repository;
  they are not locked. The workflow records `sessionInfo()` for each run.
- The official CodSpeed action runs CPU simulation on ordinary GitHub runners
  and authenticates uploads using OIDC (`id-token: write`). Import this repository
  through the [CodSpeed GitHub App](https://codspeed.io/docs/integrations/providers/github)
  to enable reports.
- Pull requests record candidate measurements; pushes to `main` provide baselines.
  The first successful default-branch run establishes the comparison baseline.
- Keep workload names and repetition counts stable. Changing R, dependencies,
  or the workload can affect measurements alongside package code changes.
- Regression thresholds and merge requirements are configured in CodSpeed and
  GitHub repository settings. The workflow itself requires successful execution
  and upload.
