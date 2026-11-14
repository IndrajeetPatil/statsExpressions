## R CMD check results

0 errors | 0 warnings | 0 notes

- Patch release (2.2.2) that fixes the test failure in the additional MKL
  check (<https://www.stats.ox.ac.uk/pub/bdr/Rblas/MKL/statsExpressions.out>).
  A test now compares Bayes factors with a numeric tolerance instead of
  requiring them to be identical. There are no user-visible changes.
