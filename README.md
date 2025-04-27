# Project Proposal for COMP4010/5120 – Project 2

## 1. One-sentence high-level goal
Create interactive, animated spatio-temporal world maps using R to visualize the global spread of COVID-19 and recovery efforts over time, showing how the virus expanded and how the world has recovered.

---

## 2. Goal and Motivation
The goal of this project is to create two parallel, animated world maps that visualize the spread of COVID-19 and the global vaccination efforts over time. The first map will display the evolution of the COVID-19 pandemic, including case numbers and deaths, while the second map will track vaccination rates globally. By showing these two maps side-by-side, we aim to allow users to compare the spread of the virus with the vaccination rollout, highlighting how vaccination efforts have influenced the course of the pandemic.

This project is interesting because it combines the visualization of critical pandemic data in a clear, accessible way. It addresses the challenge of understanding the correlation between the virus's spread and vaccination rates, providing insights into the effectiveness of global vaccination efforts. 	Through the use of interactive data, this project aims to provide a comprehensive overview of the global recovery from COVID-19 over time, while also underscoring the critical importance of timely interventions and vaccinations in managing pandemics.

---

## 3. Data Source and Collection
We will use the publicly available COVID-19 dataset provided by [Our World in Data](https://covid.ourworldindata.org/) for both the virus spread and vaccination data. The dataset contains daily COVID-19 statistics for all countries, including total deaths and vaccinations. We will import the data using R’s `readr::read_csv()` function. 

We chose this dataset because it is comprehensive and regularly updated, and it allows us to compare two critical aspects of the pandemic: the virus's spread and the vaccination efforts across the world.

**Note:** The dataset `owid-covid-data-sample.csv` uploaded to this repository is a small sample of the full dataset, provided for demonstration purposes to showcase the data format. During implementation, we will use the complete dataset.

---

## 4. Weekly Plan

| Week | Task | Team Member(s) |
|:-----|:-----|:---------------|
| **Week 1 (21/04–27/04)** | - Finalize idea and submit project proposal. | **All** |
|  | - Create and initialize GitHub repo. | **Nguyen Canh Huy** |
|  | - Filter necessary columns (country, date, cases, deaths, vaccinations). | **Nguyen Hoang Son** |
|  | - Perform basic data cleaning (handle missing values, rename columns). | **Nguyen Nhat Minh** |
| **Week 2 (27/04–04/05)** | - Create a static choropleth map showing the spread of the virus (total deaths) for a single date. | **Nguyen Hoang Son** |
|  | - Create a static choropleth map for vaccinations on the same date. | **Nguyen Canh Huy** |
|  | - Add basic animation across dates using Plotly’s `frame` feature for virus spread. | **Nguyen Nhat Minh** |
| **Week 3 (05/05–15/05)** | - Add key milestones (outbreak start, vaccine development, vaccine rollout) as annotations. | **Nguyen Hoang Son** |
|  | - Customize sliders and labels for both maps (virus map and vaccination map). | **Nguyen Nhat Minh** |
|  | - Review peer feedback and address any comments. | **All** |
| **Week 4 (16/05–30/05)** | - Polish the aesthetics (titles, captions, legends, etc.) of both maps. | **Nguyen Canh Huy** |
|  | - Clean up R scripts and add detailed comments. | **Nguyen Hoang Son** |
|  | - Write the project report. | **All** |
|  | - Create the final presentation slides. | **All** |
|  | - Final review and submission of the project package. | **All** |

