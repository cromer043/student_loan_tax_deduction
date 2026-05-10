# Student Loans and Taxes

This project now uses a source-data-to-output workflow with:

- `SIPP` target years `2018` through `2024`
- `SCF` target years `2016`, `2019`, and `2022`
- year-by-year harmonized household files
- current married-cap comparison figures in the main graph folders
- all older or appendix-style outputs routed to [`4. outmoded`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/4.%20outmoded)

The current tax logic was preserved:

- current IRS student loan interest deduction rules
- IDR-style `$0 payment` income exclusion
- MAGI phaseout rules
- married `$5,000` cap comparison
- full race analysis
- Black / Non-Black analysis

## Folder Structure

- [`0. Data`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/0.%20Data)
  Raw downloads, cached source files, and harmonized household-year files.
- [`1. Code`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/1.%20Code)
  Download, analysis, summary, and chart scripts.
- [`2. Output CSV`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/2.%20Output%20CSV)
  Current CSV outputs used by the main figures.
- [`3. Output Graphs`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/3.%20Output%20Graphs)
  Current graph outputs only.
- [`4. outmoded`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/4.%20outmoded)
  Outmoded CSV and graph outputs, including the older deduction-component figures and full-race time-series figures.

Source-specific folders:

- SIPP CSVs: [`2. Output CSV/2. A. SIPP`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/2.%20Output%20CSV/2.%20A.%20SIPP)
- SCF CSVs: [`2. Output CSV/2. B. SCF`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/2.%20Output%20CSV/2.%20B.%20SCF)
- IRS CSVs: [`2. Output CSV/2. C. IRS`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/2.%20Output%20CSV/2.%20C.%20IRS)
- SIPP graphs: [`3. Output Graphs/3. A. Sipp`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/3.%20Output%20Graphs/3.%20A.%20Sipp)
- SCF graphs: [`3. Output Graphs/3. B. SCF`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/3.%20Output%20Graphs/3.%20B.%20SCF)
- IRS graphs: [`3. Output Graphs/3. C. IRS`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/3.%20Output%20Graphs/3.%20C.%20IRS)

Outmoded mirrors:

- [`4. outmoded/2. Output CSV/2. A. SIPP`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/4.%20outmoded/2.%20Output%20CSV/2.%20A.%20SIPP)
- [`4. outmoded/2. Output CSV/2. B. SCF`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/4.%20outmoded/2.%20Output%20CSV/2.%20B.%20SCF)
- [`4. outmoded/2. Output CSV/2. C. IRS`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/4.%20outmoded/2.%20Output%20CSV/2.%20C.%20IRS)
- [`4. outmoded/3. Output Graphs/3. A. Sipp`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/4.%20outmoded/3.%20Output%20Graphs/3.%20A.%20Sipp)
- [`4. outmoded/3. Output Graphs/3. B. SCF`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/4.%20outmoded/3.%20Output%20Graphs/3.%20B.%20SCF)
- [`4. outmoded/3. Output Graphs/3. C. IRS`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/4.%20outmoded/3.%20Output%20Graphs/3.%20C.%20IRS)

## Scripts

- [`student_loan_interest_workflow_utils.R`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/1.%20Code/student_loan_interest_workflow_utils.R)
  Shared helpers for paths, year parsing, and `95%` confidence interval calculation.

- [`download_sipp_years.R`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/1.%20Code/download_sipp_years.R)
  Downloads and caches official yearly SIPP files. Best-judgment choice: this workflow uses official downloadable files rather than the SIPP API for the core microdata analysis, because the public downloadable files are more practical for full microdata plus replicate-weight work.

- [`download_scf_years.R`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/1.%20Code/download_scf_years.R)
  Uses the `scf` R package to download SCF data into year-specific cache folders.

- [`sipp_student_debt_by_race_marital_status.R`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/1.%20Code/sipp_student_debt_by_race_marital_status.R)
  Runs the SIPP analysis for a single year. It now accepts `--year=YYYY`, writes year-specific CSVs under `by-year`, saves a harmonized household-year file, and writes the current comparison CSVs used by the main graph workflow.

- [`scf_student_loan_interest_deduction_analysis.R`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/1.%20Code/scf_student_loan_interest_deduction_analysis.R)
  Runs the SCF analysis for a single year. It now accepts `--year=YYYY`, writes year-specific CSVs under `by-year`, saves a harmonized household-year file, and writes the current comparison CSVs used by the main graph workflow.

- [`build_student_debt_time_series_summaries.R`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/1.%20Code/build_student_debt_time_series_summaries.R)
  Builds the new yearly summary-statistic CSVs from harmonized household-year files.

- [`build_binding_constraint_married_cap_outputs.R`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/1.%20Code/build_binding_constraint_married_cap_outputs.R)
  Builds binding-constraint married-household outputs for the `$5,000` married-cap comparison, including Black / Non-Black subgroup gains, total modeled tax-relief tables, and the Census-population-share ratio inputs.

- [`build_filtered_interest_payment_summary_tables.R`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/1.%20Code/build_filtered_interest_payment_summary_tables.R)
  Builds source-specific CSVs and summary tables for annual interest actually paid among filtered households with positive student debt, where `interest_paid = min(accrued interest, annual_ibr_payment)`.

- [`build_net_worth_and_mortgage_summary_csvs.R`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/1.%20Code/build_net_worth_and_mortgage_summary_csvs.R)
  Builds source-specific CSVs for:
  - weighted mean and weighted median total net worth among filtered households with positive student debt, plus the percent at the current `$2,500` binding deduction constraint under the `2.75%` and `6.8%` interest assumptions
  - the weighted percent of households with mortgage debt greater than or equal to `$750,000` for all households and for the filtered positive-debt household subsets

- [`student_loan_interest_deduction_charts.R`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/1.%20Code/student_loan_interest_deduction_charts.R)
  Builds the current and outmoded graph outputs. All graph intervals are now `95% confidence intervals`, not `±1 SE`.

- [`irs_student_loan_deduction_state_maps.R`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/1.%20Code/irs_student_loan_deduction_state_maps.R)
  Downloads-ready IRS mapping workflow for the SOI state table workbook. The current main pass reads the overall state columns from [`19in55cm.xlsx`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/0.%20Data/IRS/19in55cm.xlsx) for tax year `2019`, extracts:
  - `Number of returns [1]`
  - `Student loan interest deduction: Number`
  - `Student loan interest deduction: Amount`
  and writes one state-level CSV plus two state maps to the IRS output folders. The script also accepts optional arguments so alternative IRS workbook vintages can be rendered to the outmoded IRS folders, for example the tax year `2022` workbook [`22in55cm.xlsx`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/0.%20Data/IRS/22in55cm.xlsx).

- [`run_full_workflow.R`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/1.%20Code/run_full_workflow.R)
  Convenience wrapper that downloads data, runs all source-year analyses, builds time-series CSVs, and regenerates graphs.

## How To Run

Single-year runs:

```bash
Rscript "1. Code/sipp_student_debt_by_race_marital_status.R" --year=2024
Rscript "1. Code/scf_student_loan_interest_deduction_analysis.R" --year=2022
```

Refresh cached raw data:

```bash
Rscript "1. Code/download_sipp_years.R"
Rscript "1. Code/download_scf_years.R"
```

Build time-series summaries:

```bash
Rscript "1. Code/build_student_debt_time_series_summaries.R"
```

Build binding-constraint married-cap outputs:

```bash
Rscript "1. Code/build_binding_constraint_married_cap_outputs.R"
```

Build filtered interest-payment summaries:

```bash
Rscript "1. Code/build_filtered_interest_payment_summary_tables.R"
```

Build net-worth and mortgage-threshold summary CSVs:

```bash
Rscript "1. Code/build_net_worth_and_mortgage_summary_csvs.R"
```

Regenerate graphs:

```bash
Rscript "1. Code/student_loan_interest_deduction_charts.R"
Rscript "1. Code/irs_student_loan_deduction_state_maps.R"
Rscript "1. Code/irs_student_loan_deduction_state_maps.R" --irs-file="0. Data/IRS/22in55cm.xlsx" --tax-year=2022 --outmoded=true
```

Run the full workflow:

```bash
Rscript "1. Code/run_full_workflow.R"
```

## Harmonized Household-Year Files

These are written to:

- [`0. Data/SIPP/harmonized`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/0.%20Data/SIPP/harmonized)
- [`0. Data/SCF/harmonized`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/0.%20Data/SCF/harmonized)

At minimum, the harmonized files carry the variables needed for:

- `year`
- household identifier
- `student_debt`
- positive student debt indicator
- collapsed race:
  - `White alone`
  - `Black alone`
  - `Hispanic`
  - `Other`
- `Black` / `Non-Black`
- `Married household` / `Unmarried household`
- weights
- replicate weights where available
- income / MAGI proxy
- household size
- tax deduction analysis fields

## Current Rule Details

Student loan interest deduction logic:

- `interest_paid`
  Estimated annual interest actually paid under the assumed rate scenario and the current project repayment assumption.
- repayment assumption
  All households are modeled as enrolled in `IBR`.
  - households younger than `34` are treated as post-`July 1, 2014` borrowers and pay `10%` of discretionary income over `20` years
  - households age `34` and older are treated as pre-`July 1, 2014` borrowers and pay `15%` of discretionary income over `25` years
  - discretionary income is household income above `150%` of the poverty-line proxy already used in the project
- `annual_ibr_payment = max(income - poverty-line cutoff, 0) × IBR payment rate`
- `monthly_ibr_payment = annual_ibr_payment / 12`
- `accrued_interest = student_debt × annual_interest_rate`
- `interest_paid = min(accrued_interest, annual_ibr_payment)`
- `max_potential_deduction = min(interest_paid, cap)`
- `allowable_deduction`
  Applies current MAGI phaseout rules.
- `estimated_tax_savings = allowable_deduction × marginal_tax_rate`

Current-style baseline cap:

- nonmarried households: `$2,500`
- married households: `$2,500`

Proposed married-cap policy:

- nonmarried households: `$2,500`
- married households: `$5,000`

MAGI thresholds used:

- `Single / Head of household`
  - full deduction at or below `$85,000`
  - phaseout between `$85,000` and `$100,000`
  - no deduction at or above `$100,000`
- `Married filing jointly`
  - full deduction at or below `$170,000`
  - phaseout between `$170,000` and `$200,000`
  - no deduction at or above `$200,000`

IDR-style exclusion:

- households below the approximate `$0 payment` threshold by household size are excluded
- cutoff used:
  - size `1`: `$23,500`
  - each additional household member: `+$8,200`

Family size now matters in two places:

- for the `$0 payment` exclusion
- for the modeled `IBR` discretionary-income payment cap that limits `interest_paid`

Family size still does not affect the deduction cap or the MAGI phaseout itself.

## Race Group Definitions

Main full-race analysis:

- `White alone`
- `Black alone`
- `Hispanic`
- `Other`

Black / Non-Black analysis:

- `Black`
  Any household identified as Black by race, including Black Hispanic households.
- `Non-Black`
  Everyone else.

## Time-Series Summary Definitions

These definitions are used exactly in the new yearly summary CSVs and line graphs:

1. `Percent with student debt`
   Denominator: all eligible households.
   Numerator: households with `student_debt > 0`.

2. `Median student debt`
   Sample: households with `student_debt > 0` only.

3. `Mean student debt`
   Sample: households with `student_debt > 0` only.

## Uncertainty

All graphs now use approximate `95% confidence intervals`:

- `lower_95 = estimate - 1.96 × SE`
- `upper_95 = estimate + 1.96 × SE`
- when a quantity cannot go below zero, the lower bound is floored at zero

SIPP:

- uses replicate-weight variance based on the available replicate weights

SCF:

- combines SCF replicate-weight sampling variance with imputation variance across implicates using the standard `6/5` rule already used in the project

## Output Rules

Main graph folders now keep only the current memo-order figures:

1. `Percent of Households with Student Debt by Black and Non-Black Group`
2. `Mean Student Debt Among Debt Holders by Black and Non-Black Group`
3. `$2,500 Cap Binds Differently for Black and Non-Black Borrowers`
4. `A Higher Married Cap Raises Deductions and Tax Savings for Married Black Households`

Older graphs still generate, but they now write to `4. outmoded` instead of the main graph folders.

## New Binding-Constraint Outputs

These files are now written from the harmonized latest-year source files:

- [`2. Output CSV/2. A. SIPP/sipp_binding_constraint_married_cap_black_nonblack.csv`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/2.%20Output%20CSV/2.%20A.%20SIPP/sipp_binding_constraint_married_cap_black_nonblack.csv)
- [`2. Output CSV/2. B. SCF/scf_binding_constraint_married_cap_black_nonblack.csv`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/2.%20Output%20CSV/2.%20B.%20SCF/scf_binding_constraint_married_cap_black_nonblack.csv)
- [`2. Output CSV/binding_constraint_tax_relief_summary.csv`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/2.%20Output%20CSV/binding_constraint_tax_relief_summary.csv)
- [`2. Output CSV/black_population_share_census_b02009.csv`](/Users/carlromer/Documents/Brookings/Student%20loans%20and%20taxes/2.%20Output%20CSV/black_population_share_census_b02009.csv)

The Census population share file uses:

- `2024 ACS 5-year`
- `B02009_001E`: Black alone or in combination with one or more other races
- `B01003_001E`: total population

## Current Local State

The local caches now include the full targeted source years:

- SIPP `2018` through `2024`
- SCF `2016`, `2019`, and `2022`

So the harmonized files, yearly summary CSVs, and time-series graph workflow can now produce actual multi-year outputs rather than single-year point estimates.
