# R Programming — Educational Archive

Exercises in regression, classification, multivariate statistics, time series and cricket-calendar visualisation.

Selected files: [Cricket calendar](Cric%20Calendar%2024-25.R), [Time series](Time%20Series.R), [Logistic regression](Logit%20Reg.R), [MVA](MVA.R).

Open in RStudio. Scripts include independent demonstrations and may require multiple packages; they have not been collectively tested.

## Known correctness issues (pending repair)
- `Logit Reg.R`: `predict(...,type="response")` yields probabilities, so odds should be `p/(1-p)`, not `exp(p)`.
- `Time Series.R`: the function called Schmidt–Phillips actually uses t-tests, not the Schmidt–Phillips procedure.
- Cricket-calendar inputs are manually entered and are **not** verified as an automated data pipeline.

## Upgrade plan
- [ ] Fix methods/naming, split independent demonstrations and add validation
- [ ] Add dataset provenance, package versions and reproducibility instructions
- [ ] Turn mature cricket work into [IPL Analytics Pipeline](https://github.com/HiteshS37/ipl-analytics-pipeline)

Author: Hitesh Srinivas
