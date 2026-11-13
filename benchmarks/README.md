# Benchmarking

The [CodSpeed CLI](https://github.com/CodSpeedHQ/codspeed) measures the main statistical
entry points through its generic executable harness. Each named command
runs five package calls and checks the returned statistics and expression column.
Measurements include R startup and dependency loading, so they describe these
complete workloads. Native profiles may show R interpreter frames rather than
individual R functions.

## Coverage policy

Benchmark the main statistical API, rather than every public export. Exclude
`meta_analysis()` and all other meta-analysis workloads. Helpers, tidying,
expression formatting, and data conversion functions are not separate targets.
Use deterministic, representative workloads with fixed datasets; keep names and
repetition counts stable and update this catalogue when the main API changes.

| Target | Function |
| --- | --- |
| `centrality-parametric` | `centrality_description()` |
| `one-sample-parametric` | `one_sample_test()` |
| `two-sample-parametric` | `two_sample_test()` |
| `anova-parametric` | `oneway_anova()` |
| `correlation-parametric` | `corr_test()` |
| `contingency-parametric` | `contingency_table()` |
| `pairwise-parametric` | `pairwise_comparisons()` |
| `pairwise-contingency` | `pairwise_contingency_table()` |

## Run locally

Install the checkout and run each workload from the repository root:

```sh
R CMD INSTALL .
Rscript --vanilla benchmarks/run.R centrality-parametric
Rscript --vanilla benchmarks/run.R one-sample-parametric
Rscript --vanilla benchmarks/run.R two-sample-parametric
Rscript --vanilla benchmarks/run.R anova-parametric
Rscript --vanilla benchmarks/run.R correlation-parametric
Rscript --vanilla benchmarks/run.R contingency-parametric
Rscript --vanilla benchmarks/run.R pairwise-parametric
Rscript --vanilla benchmarks/run.R pairwise-contingency
```

These commands check execution and output without collecting CodSpeed measurements.
On a supported Linux machine with the CodSpeed CLI installed, use
`codspeed run --mode simulation` to measure the targets in `codspeed.yml`.

## CI and comparisons

- The `Benchmarks` caller uses the shared `r-benchmarks.yaml` workflow from
  `IndrajeetPatil/workflows` at `ci/r-benchmarks-workflow` for coordinated PR
  validation. Use `@main` once the shared workflow is merged. The shared job uses Ubuntu 24.04, R 4.6.1, and one thread for BLAS
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

CPU simulation excludes system calls. CodSpeed flags these complete R commands
as dominated by system calls, so the measurements understate their total elapsed
cost. Use repeated successful runs to check operational stability; matching
rounded values alone does not establish sub-percent variance.
