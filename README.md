# Forecasting Project: Comparing Auto-ARIMA and Neural Network Autoregression (NNAR)

This project analyzes the monthly unemployment rate in Maryland (1980–2024) using recursive forecasting methods.  
The goal is to compare traditional time-series approaches (Auto-ARIMA, AR2) with neural network autoregression (NNAR), including fixed and dynamic training window models.

##  Files Included
- **Forecasting_project.qmd** — Source Quarto document containing full analysis  
- **Forecasting_project.html** — Rendered project report (self-contained HTML)  
- **Data/** — Contains the dataset used for the analysis  
- **scripts/** — Custom forecasting and evaluation functions  

##  Methods
- Auto-ARIMA (non-stepwise, full search)
- AR(2) with seasonal components
- Fixed-window NNAR (selected network architecture)
- Dynamic-window NNAR that adapts training length during COVID shifts

##  Main Findings
- The NNAR model trained on the full window outperformed the ARIMA models overall.  
- During structural breaks (e.g., COVID), fixed NNAR remained the most robust.  

##  Project Report
You can view the rendered HTML report here:  
[Forecasting Project Report](https://lukeharold.github.io/projects/Forecasting_project.html)**

## Author
Luke Harold  
MA Economics, University of Alberta
