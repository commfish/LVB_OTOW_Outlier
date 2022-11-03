---
title: "Ludwig von Bertalanffy and Otolith Weight at Age Outlier Analysis"
author: "Kevin McNeel"
date: "2022-10-07"
output: html_document
---

```{r setup, include=FALSE, echo=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

## General Use

This Project was initiated to find outliers in fisheries data for both length at age and otolith weight at age. For length at age models, we used a LVB model following modeling from Ogle (2015) and Monte Carlo prediction intervals using package {predict}. Otolith weight models are Ln:Ln models based on exponential growth models outlined in Ogle (2015).The LVB model_2.R and Length_Weight model_2.R are standalone files that can be used to detect outliers for either length:age, or length:weight data. The LVB and LnLn otolith model files require the Output_Outliers script to finalize the dataframe and flag potential errors.  

## LVB Models

This script runs classic LVB models based on the target species. The initial code is set to rename columns into names used in the code. Also, the script will try to automatically convert cm to mm based on order of magnitude. If you are using both modeling scripts, you only have to change the column names once. There is also code to identify the target species, so be careful is this isn't set or if there isn't a column in your dataframe.

The script will estimate starting values and is set to run generic and sex specific models. There are lines to output the model estimates to the clipboard to paste and compare those. The prediction intervals can be set by changing the alpha parameter, but are set to 99%. This script then plots the data with the intervals and exports the intervals to the clipboard to paste into an excel or text file.

## Otolith Models

This script runs a classic exponential models with standard prediction intervals. Similar to the LVB script, it starts with renaming columns and converting cm to mm. That is not necessary, but it prevents users from needing to do that step again for either code. Within the standard model, a decimal is added after the age to make use of potential age-zero fish. I have not found that the shift impacts the model results for the other ages. I also added an additional intercept to the model to help fit the orgin. If your models do not need that, please consider removing it. 

Similar to the LVB model, you can change the alpha level of the prediction interval. Unlike the LVB model, the alpha level is inverted (0.01 on the LVB model is equivalent to 0.99 on the Ln:Ln model). The model will also go through plots of the cutoffs over the data and exports the size at age to clipboard to add to the other model results. 

## Output Outliers

This script takes all of the lookup tables and applies the limits to each fish based on the length or otolith weight. I tried to include helpful summaries of the data to find fish with missing data points and to identify issues across collections. I also included script to combine the length and otolith plots using {cowplot}. At the end of this script is an output for tables that mark individuals as being outliers or not. 
