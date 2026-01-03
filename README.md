# Most Valuable Player (MVP) Modelling in Amateur Cricket

This repository contains the full codebase and supporting materials for my MSc Data Science dissertation, which focuses on developing a data-driven **Most Valuable Player (MVP)** framework for the North East Durham Cricket League.

The project combines statistical analysis, machine learning, and domain-specific feature engineering to evaluate player performance in a fair, interpretable, and role-aware manner.

## Project Objective
Traditional cricket performance metrics often fail to capture a player’s true contribution, especially across different roles (batters, bowlers, all-rounders).  
The objective of this project is to:

- Design a **role-aware MVP scoring framework**
- Use **unsupervised learning** to identify natural player groupings
- Apply **machine learning** to understand feature importance
- Produce interpretable, decision-ready outputs rather than black-box rankings

## Data
- Source: North East Durham Cricket League (DNECL)
- Format: Season-level player statistics (Excel)
- Scope: Batting, bowling, and derived performance metrics

All data preprocessing, cleaning, and validation steps are reproducible within this repository.

## Methodology Overview
The project follows an end-to-end analytical pipeline:

1. **Data Cleaning & Preparation**
   - Handling missing values
   - Standardisation of performance metrics
   - Creation of derived features (e.g. efficiency and impact measures)

2. **Feature Engineering**
   - Role-specific metrics for batters, bowlers, and all-rounders
   - Normalisation to allow fair cross-role comparison

3. **Dimensionality Reduction**
   - Principal Component Analysis (PCA) to:
     - Reduce multicollinearity
     - Improve clustering stability
     - Enhance interpretability

4. **Unsupervised Learning**
   - K-means clustering to identify player archetypes
   - Cluster validation using silhouette scores
   - Role-based segmentation of players

5. **MVP Scoring Framework**
   - Weighted scoring model informed by:
     - PCA loadings
     - Domain understanding
     - Robustness checks

6. **Model Interpretation & Validation**
   - Random Forest feature importance
   - Bootstrap-based confidence intervals for MVP scores

7. **Visualisation & Delivery**
   - Clear visual summaries of clusters and scores
   - Interactive R Shiny dashboard for exploration and comparison

## Tools & Technologies
- **Languages:** R
- **Libraries:** tidyverse, FactoMineR, cluster, randomForest, caret, shiny
- **Techniques:** PCA, K-means clustering, Random Forests, bootstrapping


## Key Outcomes
- A transparent and reproducible MVP scoring system
- Clear identification of player roles and archetypes
- Evidence that role-aware metrics outperform raw aggregate statistics
- An interactive dashboard designed for league officials and analysts

## Why This Matters
This project demonstrates how **machine learning and analytics can support decision-making**, not replace it.  
Rather than optimising for model accuracy alone, the focus is on **interpretability, fairness, and real-world usability** — principles that are equally relevant in product, analytics, and CRM contexts.

## 👤 Author
**Akbar Jamal Khan**  
MSc Data Science (Distinction), Durham University
## 📂 Repository Structure

