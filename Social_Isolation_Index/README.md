# 📘 Social Isolation Index (MCA-Based)

This folder contains the construction of the **Social Isolation Index**, derived using *Multiple Correspondence Analysis (MCA)* to summarize the underlying structure of categorical indicators related to social isolation and adverse interpersonal experiences.

## 🧩 Variables Included
Five nominal variables were evaluated:

- Frequency of feeling silenced or excluded (`uls_2`)
- Frequency of feeling completely alone (`uls_3`)
- Exposure to mobility restriction by others (`msoc_forb`)
- Experiences of humiliation (`msoc_humi`)
- Deprivation of basic needs despite available resources (`msoc_prov`)

All variables and categories are listed in **Supplementary Table S1**.

---

##  MCA Procedure
MCA decomposed the joint variability of the indicators into orthogonal dimensions:

- **Dimension 1 (Dim 1)** captured the dominant structure, explaining **38.5%** of the inertia (Table S2).
- **Category coordinates** on Dim 1 and Dim 2 showed that:
  - Higher positive loadings corresponded to **greater social isolation and interpersonal adversity**
  - Negative loadings indicated **lower exposure** (Table S3)

All five variables contributed strongly to Dim 1 (η² = 1.00), confirming that this axis effectively summarizes the shared structure (Table S4).

---

##  Index Score
Individual MCA scores on **Dim 1** were extracted and used as the **composite Social Isolation Index**.

- Higher scores = profile characterized by **greater isolation**
- Scores were **standardized (mean = 0, SD = 1)** for comparability across models
- Descriptive statistics are reported in **Supplementary Table S5**

This standardized Dim 1 score was included as a continuous predictor in the main statistical analyses.

---

##  Files in This Folder
- `MCA_social_isolation_index.xlsx` → Input variables and categories  
- `R_code_Social_Isolation_Index.R` → R script for MCA and index extraction  
- `README.md` → Documentation for index construction  

---

📌 *For replication or further methodological details, refer to the corresponding tables in the Supplementary Material.*

