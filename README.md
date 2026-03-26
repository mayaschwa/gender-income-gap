# Gender Income Gap by Academic Field
*Final Paper for POLI_SCI 312 at Northwestern University - March 2025*

## Overview
This project explores the relationship between gender composition in academic departments and post-graduation income outcomes. While women earn less than men on average, this analysis investigates whether fields with higher proportions of female students are systematically associated with lower income levels, independent of individual skill or educational effort. The study uses regression models to evaluate how structural educational factors may contribute to long-term economic disparities.

## Methods
- Data cleaning and preprocessing in R  
- Exploratory data analysis (EDA)  
- Calculation of key independent variable: percent female in field  
- Multivariate ordinary least squares (OLS) regression with control variables including socioeconomic status, race/ethnicity, geographic location, and sex  
- Nonlinear and interaction terms to test robustness of results  
- Visualizations and summary tables for regression outputs  

## Key Findings
- Fields with higher female representation are associated with slightly lower post-graduation income  
- After controlling for confounding variables, a 1% increase in female proportion corresponds to an approximate 1.88% decrease in income 
- Nonlinear and interaction terms indicate complex relationships with socioeconomic status and income  
- Structural differences in field-level earnings suggest that labor markets may systematically undervalue work associated with women  

## Data Sources
- [IPUMS USA Dataset, Version 13.0](https://doi.org/10.18128/D010.V13.0) – Integrated Public Use Microdata Series, 2020–2023  

## Files
- `gender_income_code.R` – R scripts used for data cleaning, regression modeling, and visualizations  
- `gender_income_report.pdf` – Final paper including analysis and conclusions
