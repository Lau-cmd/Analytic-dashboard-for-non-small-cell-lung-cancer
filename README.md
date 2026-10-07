# Analytic-dashboard-for-non-small-cell-lung-cancer
Real-World Evidence (RWE) Observational Cohort &amp; Survival Analysis in NSCLC.

* **Clinical Question:** Evaluate the real-world comparative effectiveness and direct economic impact of *Novel Targeted Immunotherapy* versus *Standard of Care Chemotherapy*.

---

To emulate realistic clinical observational conditions without privacy constraints, the application dynamically generates a synthetic patient cohort (n = 1,000) following empirical epidemiological distributions from the **NIH/NCI Surveillance, Epidemiology, and End Results (SEER)** program.

---

1. **Univariate Hypothesis Testing:**
   * **Welch Two-Sample Independent t-Test:** Confirms baseline biomarker balance across treatment arms to evaluate selection bias.
   * **Pearson's chi^2 Test of Independence:** Assesses categorical association between treatment arms and Objective Response Rate.

2. **Multivariable Predictive Modeling:**
   * **Logistic Regression:** Models odds of achieving clinical objective response adjusted for Age, Gender, TNM Stage, and PD-L1 expression. Includes interactive Forest Plots.
   * **Kaplan-Meier & Cox Proportional Hazards:** Calculates overall survival probabilities and hazard ratios for mortality risk reduction over time.

3. **Health Economics & Market Access (HEOR):**
   * **Cost-Offset Trajectory:** Analyzes how upfront drug acquisition costs are balanced by reductions in 24-month emergency readmissions and palliative care resource utilization.
   * **Managed Entry Agreements (MEAs):** Formulates value-based reimbursement recommendations for payers.

---


* **Language & Framework:** R, R Shiny (`page_navbar`, `bslib` Bootstrap 5)
* **Data Wrangling:** `dplyr`, `broom`
* **Visualization:** `ggplot2`, `plotly` (Interactive charts)
* **Biostatistics & Survival:** `survival`, `survminer`

---

# Clone this repository or download app.R
# Open RStudio and run:
shiny::runApp("app.R")
