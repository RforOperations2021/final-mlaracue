# NYPD Explorer: Analysis of last six months of crime data in NYC

The application can be seen in [this](https://mlaracue.shinyapps.io/nypd-explorer/) link

## About

This application is an explorer of NYPD crime data for the last six months (from July 1st, 2020 to December 31st, 2020). The Data is retrieved directly from [Socrata API](https://dev.socrata.com/foundry/data.cityofnewyork.us/5uac-w243) using queries made based on user inputs. The app has three main components:

1. **Sidebar with filters.** The user can search data based on date, borough, and level of offense. For the data, a maximum of 30/31 days is possible to avoid errors on requests made to the API (when the Data is too large, it may return an error). 
2. **Plots.** The main panel has four interactive plots and one map with two different layers. The plot on the top of the page is a time-series where users can view the number of crimes by date and hour. On the bottom, users can see the main map with two options: a heatmap and circle markers. On the right-hand side, users can see two plots: one for the number of crimes by day of the week and the other for the total number of crimes by the hour of the day. 
3. **Data.** On the secondary tab, users can see the data that was downloaded from the API.

## The Data
This dataset includes all valid felony, misdemeanor, and violation crimes reported to the New York City Police Department (NYPD) for all complete quarters so far this year (2019). For the app, only the second semester of 2020 is being used.

## Notes

### How sample size is being managed
Sometimes, an user can make a request of too much data. So, the time execution is being controlled by using the `withTimeout` function from the `R.utils` package. Hence, when the time execution is greater than one (1) second, a *sample size* option is activated. Additionally, When sample size is greater than 1,000, a warning is sent to the user saying that response time might be large.

### Ranges for heatmap legend
The maximum number of crimes is unknown. Therefore, I approximate the maximum number of crimes to the nearest tenths or hundreds and divide this number by 5. That way, I can programmatically create the ranges regardless of the values.