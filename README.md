
# Research on the Financial Effects of BIM in PPP Projects

A reproducible R-based research repository on how Building Information Modeling (BIM) affects the financial performance of public-private partnership (PPP) projects through operating efficiency, risk reduction, and managerial flexibility.

This repository is not just a collection of scripts. It is a structured research handoff designed to make the full analytical workflow transparent, reproducible, and defensible.

---

## Project overview

This project evaluates the financial effects of BIM adoption in PPP infrastructure projects using a multi-layer valuation framework.

The analysis combines:

- project-level PPP and operating data,
- survey-based and literature-based evidence on BIM effects,
- deterministic DCF scenarios,
- Monte Carlo simulation,
- WACC sensitivity analysis,
- meta-regression,
- and stylized real-option valuation.

The final goal is to decompose BIM-related project value into distinct economic channels rather than treating BIM as a single undifferentiated benefit.

---

## Research question

How does BIM create measurable financial value in PPP projects, and through which channels can that value be translated into project-level valuation metrics?

---

## Core analytical idea

The repository treats BIM-related value as a combination of four layers:

1. **Baseline project value**  
   The initial NPV of each PPP project under baseline assumptions.

2. **Value efficiency**  
   Additional value created through OPEX improvements and operational savings associated with BIM.

3. **Value of risk reduction**  
   Additional value created through a stylized reduction in WACC, interpreted as improved transparency, lifecycle control, and lower uncertainty.

4. **Value of managerial flexibility**  
   Additional value created by the option to deploy an advanced BIM module in the future if conditions become favorable.

This produces a final BIM value decomposition instead of a single opaque valuation number.

---

## Repository structure

```text
research-bim-financial-results/
├── README.md
├── LICENSE
├── .gitignore
├── research-bim-financial-results.Rproj
│
├── R/
│   ├── 00_packages.R
│   ├── 01_functions_general.R
│   ├── 02_functions_dcf.R
│   ├── 03_functions_plots.R
│   └── 04_theme.R
│
├── scripts/
│   ├── 00_run_all.R
│   ├── 01_load_and_clean_data.R
│   ├── 02_descriptive_analysis.R
│   ├── 03_project_level_opex_and_dcf.R
│   ├── 04_bim_effect_calibration.R
│   ├── 05_scenario_analysis.R
│   ├── 06_monte_carlo_opex.R
│   ├── 07_monte_carlo_opex_wacc.R
│   ├── 08_wacc_sensitivity.R
│   ├── 09_meta_regression.R
│   ├── 10_real_options.R
│   └── 11_value_decomposition.R
│
├── data/
│   ├── raw/
│   │   ├── Dataset 1_Thesis.xlsx
│   │   ├── Dataset 2_Thesis.xlsx
│   │   ├── Dataset 3_Thesis.xlsx
│   │   └── Dataset 4_Thesis.xlsx
│   └── README.md
│
├── outputs/
│   ├── rds/
│   ├── tables/
│   └── figures/
│
├── notebooks/
│   └── main_analysis.Rmd
│
├── docs/
│   ├── methodology_workflow.md
│   └── variable_dictionary.md
│
└── memo/
    └── interpretation_and_defense_notes.md
```
---

## Analytical workflow

The repository is organized as a full research pipeline.

### 1. Data loading and cleaning

`01_load_and_clean_data.R`

- reads the four thesis datasets,

- standardizes column names,

- derives analytical variables,

- saves cleaned .rds objects for downstream use.

### 2. Descriptive layer

`02_descriptive_analysis.R`

- describes PPP versus non-PPP distribution,

- summarizes BIM adoption across stages,

- compares delays and CAPEX overruns.

### 3. Baseline DCF layer

`03_project_level_opex_and_dcf.R`

- aggregates annual operating data to project level,

- merges project metadata,

- computes baseline DCF inputs and baseline NPV.

### 4. BIM effect calibration

`04_bim_effect_calibration.R`

- summarizes evidence from company survey and literature,

- extracts OPEX-relevant effects,

- calibrates low / base / high BIM effect scenarios.

### 5. Deterministic scenario analysis

`05_scenario_analysis.R`

- applies calibrated OPEX scenarios,

- compares actual BIM adoption and hypothetical full adoption,

- estimates deterministic NPV uplifts.

### 6. Monte Carlo: OPEX layer

`06_monte_carlo_opex.R`

- simulates uncertainty in BIM-induced OPEX savings,

- estimates project-level NPV distributions,

- derives the value efficiency component.

### 7. Monte Carlo: joint OPEX + WACC layer

`07_monte_carlo_opex_wacc.R`

- simulates joint uncertainty in operational gains and discount rate,

- allows for correlation between efficiency improvement and risk reduction,

- builds the joint stochastic valuation layer.

### 8. WACC sensitivity layer

`08_wacc_sensitivity.R`

- isolates the valuation effect of a stylized reduction in WACC,

- compares actual and hypothetical discount-rate improvements.

### 9. Meta-regression layer

`09_meta_regression.R`

- estimates multiple meta-regression specifications,

- examines how stage, mechanism, country, and time relate to reported BIM effects,

- generates explanatory and calibration-oriented evidence.

### 10. Real-options layer

`10_real_options.R`

- estimates the value of managerial flexibility,

- models a stylized option to install an advanced BIM module at a future decision date.

### 11. Final value decomposition

`11_value_decomposition.R`

- combines baseline NPV, efficiency value, risk-reduction value, and real-option value,

- produces the final BIM value decomposition for each PPP project.

---

## What the client receives at the end of the project

Not a random archive of files, but a structured research handoff where every stage of the analysis is transparent, reproducible, and ready for academic review or portfolio presentation.

- `/data` — raw and cleaned datasets, variable descriptions, and documented transformations

- `/scripts` — modular analytical workflow, split by research stage

- `/outputs` — tables, figures, coefficients, simulation outputs, and valuation summaries

- `/R` — reusable helper functions, financial functions, plotting utilities, and common theme definitions

- `/memo` — concise notes on how to defend the method, interpret the results, and communicate limitations honestly

This is especially important for students and researchers: a supervisor sees a transparent research process rather than a “magic script with no authorship trail.”

---

## Reproducibility

This repository is designed to be reproducible.

### Run the full pipeline

From the project root directory:

```r
source("scripts/00_run_all.R")
```

### Or run step by step

You can also execute individual scripts in sequence, depending on the analytical layer you want to inspect.

---

## Main outputs

Key outputs are saved to:

- `outputs/rds/` — reusable processed objects

- `outputs/tables/` — exported tables for thesis chapters and appendices

- `outputs/figures/` — publication-ready visualizations

---

## Methodological note

This repository combines deterministic and stochastic valuation tools in a structured way. Some layers are explicitly stylized rather than fully structural.

In particular:

- the WACC reduction is treated as a stylized valuation assumption,

- the real-option layer is a managerial flexibility approximation rather than a market-traded contingent-claims model,

- the meta-regression layer is explanatory and calibration-oriented rather than causal.

These choices are intentional and are documented to keep the analytical process transparent and defensible.

---

## Portfolio value

This repository demonstrates:

- reproducible research workflow in R,

- structured project handoff,

- project-level financial modeling,

- scenario analysis and Monte Carlo simulation,

- evidence calibration from literature and expert mapping,

- academic and portfolio-grade presentation of results.

---

## Limitations

The project should be interpreted with appropriate caution.

Main limitations include:

- small number of PPP projects in the final project-level valuation sample,

- stylized treatment of WACC and real-option assumptions,

- dependence on mapped effect sizes from survey and literature evidence,

- partial aggregation of heterogeneous BIM effects into unified financial channels.

These limitations are not hidden; they are part of the research design and interpretation strategy.

---

## Author

Evgenii Azarov

Research focus: BIM, PPP valuation, DCF modeling, scenario analysis, Monte Carlo methods, and financial interpretation of digital transformation effects.
    
    
