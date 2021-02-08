# ![alt text](favicon-32x32.png) COVID-19 Dashboard


[![View on Github](github.svg)](https://github.com/amannj/COVID-19)

- Kindly report any issues you may encounter to the [designated GitHub page](https://github.com/amannj/COVID-19/issues). Thank you.
- Created by [(c) 2021 by Juergen Amann](https://amannj.github.io).


--------------


**Data:**

- Data for cross-country analysis taken from the [COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE](https://github.com/CSSEGISandData/COVID-19). 
- Austrian data taken from [BMSGPK](https://www.sozialministerium.at/Informationen-zum-Coronavirus/Neuartiges-Coronavirus-(2019-nCov).html), [Gesundheitsministerium.at](https://info.gesundheitsministerium.at/) and
[derStandard](https://www.derstandard.at/story/2000115810293/aktuelle-zahlen-zum-coronavirus?ref=cta_red_s1). Population by district from [Statistik Austria](https://www.statistik.at/web_de/klassifikationen/regionale_gliederungen/politische_bezirke/index.html).
- Population data extracted from the [World Development Indicators (WDI) data base](https://databank.worldbank.org/reports.aspx?source=World-Development-Indicators) using package [WDI](https://github.com/vincentarelbundock/WDI).

**Shapefile Austria:**

- Shapefile for Austria taken from [EEA](https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/austria-shapefile).
- Shapefile was modified using [QGIS](https://www.qgis.org/en/site/) to incorporate the most recent district re-classification in which district 'Wien Umgebung' was resolved; [click here for more information](https://en.wikipedia.org/wiki/Wien-Umgebung_District).

**Favicon:**

- Favicon taken from https://icons8.com/.

**Acknowledgements:** 

- Inspiration: Yanchang Zhao, COVID-19 Data Analysis with Tidyverse and Ggplot2 – Worldwide. RDataMining.com, 2020. URL: [http://www.rdatamining.com/docs/Coronavirus-data-analysis-world.pdf](http://www.rdatamining.com/docs/Coronavirus-data-analysis-world.pdf).

- Data preparation using the [Tidyverse](https://www.tidyverse.org/). Visualisation with [flexdashboard](https://rmarkdown.rstudio.com/flexdashboard/index.html).
