# Who Buys From an Online Ad? Logistic Regression in R

A statistical analysis of 400 people who saw an online ad, to find out which customer traits predict whether they go on to buy.

**Tools:** R (`glm` logistic regression, `ggplot2`, `psych`, `arm`, `ResourceSelection`)

## Business question

Should the ad be targeted by gender, by age, or both?

## Approach

1. Explored the data (400 users, 36% purchased) and checked for missing values.
2. Fit a logistic regression of **Purchased ~ Gender + Age**.
3. Dropped the variable that wasn't significant and refit **Purchased ~ Age**, then compared the two models with AIC and BIC.
4. Converted the age coefficient into an odds ratio with a 95% confidence interval.
5. Checked the model with a likelihood ratio test, Pearson and deviance residuals, and a Hosmer–Lemeshow goodness-of-fit test.
6. Plotted the fitted purchase-probability curve against age.

## Key findings

- **Gender does not predict purchase** (p = 0.73). Purchase rates were similar for women (38%) and men (34%).
- **Age is a strong predictor** (p < 0.001). Each additional year of age raises the odds of buying by about **21%** (odds ratio 1.21, 95% CI 1.16–1.25).
- **The age-only model is the better model.** It has a lower AIC (340.3 vs 342.1) and BIC, and a likelihood ratio test against the null model is highly significant.
- **Purchase probability climbs sharply with age:**

| Age | Predicted chance of purchase |
|---|---|
| 25 | 3% |
| 35 | 19% |
| 45 | 61% |
| 55 | 91% |

  The tipping point, where a purchase becomes more likely than not, is about **age 43**.
- **The model classifies 84% of users correctly** using a 50% cut-off.

## Recommendations

1. **Target by age, not gender.** Focus ad spend on people over about 40, where conversion is most likely.
2. **Rethink the offer for under-30s.** Only 5% of 18–30 year olds bought, so this group needs a different product, message or price point rather than more of the same ad.

## Next steps

- Add `EstimatedSalary` (already in `social.csv`) to test whether income explains purchase beyond age.
- Validate on a held-out test set instead of the training data.

## How to run

Run `R-Purchasing.R` from the repo folder; it reads `social.csv` directly. Install the packages first:

```r
install.packages(c("psych", "ggplot2", "arm", "ResourceSelection"))
```

## Files

| File | Contents |
|---|---|
| `R-Purchasing.R` | Full analysis: exploration, models, odds ratios, diagnostics, plots |
| `social.csv` | 400 users: User ID, Gender, Age, EstimatedSalary, Purchased |
