# EdgeEffects

## Data files

To assess the impacts of edge creation on forests globally.
All data extracted from peer-reviewed studies.

`mergedrefined7.csv` is qualitative data on studies (location, scientific design, etc)

`vardata.csv` is quantitative data from studies (air temperature, relative humidity, etc), expressed in percent change (percent--diff) from most internal forest point observed.
`vardata2.csv` expresses the same data with factors instead of strings for matrix type, etc and with log1p on proportions (in `outputs\`)

`withbroad.csv` contains broad climate categories (temperate, tropical, boreal) to add to mergedrefined7

`allvariances.csv` is quantitative data on all variance types, including standard errors, confidence intervals, and standard deviations reported (in `outputs\`)

## Scripts

These files are manipulated in:

-   **qualitative.R** for qualtitative data only (how/where/when/by whom were the data collected? which journals? etc.)

-   **quantitative.R** for quantitative data only (which measurements were taken?)

-   **qualquant.R** mixture of qualitative and quantitative data (did the methods influence measurements?)

Relevant visualizations are included with their data type/s.

A writeup is available at [esparsons.com](http://www.esparsons.com/abiotic-edge-effects/).
Academic paper forthcoming.
