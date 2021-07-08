# Flood-Prediction-in-Malawi

On 14 March 2019, tropical Cyclone Idai made landfall at the port of Beira, Mozambique, before moving across the region. Millions of people in Malawi, Mozambique and Zimbabwe have been affected by what is the worst natural disaster to hit southern Africa in at least two decades.

In recent decades, countries across Africa have experienced an increase in the frequency and severity of floods. Malawi has been hit with major floods in 2015 and again in 2019. In fact, between 1946 and 2013, floods accounted for 48% of major disasters in Malawi. The Lower Shire Valley in southern Malawi, bordering Mozambique, composed of Chikwawa and Nsanje Districts is the area most prone to flooding.

The objective of this challenge is to build a machine learning model that helps predict the location and extent of floods in southern Malawi.

This competition is sponsored by Arm and UNICEF as part of the 2030Vision initiative.

Southern Malawi experienced major flooding in 2015 and again in 2019 with cyclone Idai. Approximate dates of impact are 13 January 2015 and 14 March 2019, respectively.

We have broken up the map of southern Malawi into approximately 1 km sq rectangles. Each rectangle has a unique ID. Each rectangle has been assigned a "target" value which is the fraction (percentage) of that rectangle that was flooded in 2015.

For this competition, the training data is the flood extent in 2015 in southern Malawi, however, you are encouraged to source other flood data for other nearby regions and other historic floods to train your model. (Just be sure to propose any new datasets that are not listed here to Zindi at zindi@zindi.africa for approval.)

The test data to measure the accuracy of your model is the flood extent in southern Malawi in 2019.

Each unique rectangle also has some additional features that we have already extracted for you. Although we encourage you to add more yourself, these features are included as a starting point. They are:

Elevation. Mean elevation over the rectangle, based on this dataset in Google Earth Engine.
Dominant Land Cover Type. Most areas are predominantly grasslands, savannah or cropland. You can find the full list of land cover types here in the ‘LC_Type1 Class Table’.
Weekly Precipitation. Historical rainfall data for each rectangle, for 18 weeks beginning 2 months before the flooding. Rainfall estimates from this dataset in Google Earth Engine.
Train.csv has the target variable for 2015, along with the above features (including rainfall for both the 2015 and 2019 flood events). The submission file should have the predicted target for 2019 and cover the same locations as Train. The X, Y coordinates given represent a rectangle 0.01 degrees on each side, centered on that X-Y location.

The target is the percentage of the given rectangle that was flooded, with a value between 0 and 1.

In addition to the features we have provided in the train and test CSV, you are free to extract additional datasets and features from the sites listed below:

Google Earth Engine
http://www.diva-gis.org/
Think about features such as land cover, elevation and slope, soil properties etc. that will affect how water moves in the environment. You may also use data on weather and rainfall leading up to and during the flooding.

Note that you cannot use images to detect the actual flood extent in the test data. In other words, this is not a computer vision challenge for identifying actual flooding. Any solutions that use models to detect actual flood extent from actual flood images in southern Malawi in 2019 will be disqualified. However, you may use imagery from before the flood events (imagery must be from at least one month before the flooding) to extract features you think might be useful to your model.

Finally, you can also propose other publicly-available datasets or data sources to us. We will review and approve your proposals and add them to the official list of accepted datasets above.

# The files for download are:
Train.csv - has the target variable and other features for each rectangle for the flood in 2015. You will use this data to train your model.
Test.csv - there is no test file as you will be using the same coordinates as the Train file.
SampleSubmission.csv - is an example of what your submission file should look like. The order of the rows does not matter, but the names of the Square_ID must be correct. Where Square is a rectangle.
To propose additional datasets, email zindi@zindi.africa. New data sets will not be accepted after 8 May 2020.

Additional datasets
Please note that you cannot use data during the 2019 cyclones or afterward. If you are using rainfall data you can use it for 18 weeks beginning 2 months before the 2019 cyclone.

Landsat imagery

https://www.usgs.gov/land-resources/nli/landsat/data-tools
Historic rainfall and temperature data

https://climateknowledgeportal.worldbank.org/download-data
http://chelsa-climate.org/
https://earthobservatory.nasa.gov/global-maps/MOD_LSTD_M
https://www.worldweatheronline.com/developer/api/historical-weather-api.aspx
https://openweathermap.org/
https://neo.sci.gsfc.nasa.gov/
Malawi geospatial data

http://www.masdap.mw/
Soil data

https://www.isric.org/explore/wosis/accessing-wosis-derived-datasets
https://esdac.jrc.ec.europa.eu/search/node/malawi
Landcover data

http://gis1.servirglobal.net:8080/geonetwork/srv/eng/catalog.search;jsessionid=C176A5DC99F9FCDDBBFA19997242AE4A#/metadata/4429e427-c117-43c7-8d8d-406160448ba7
http://geoportal.rcmrd.org/layers/servir%3Amalawi_landcover_2000_scheme_i
https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-shuttle-radar-topography-mission-srtm-non?qt-science_center_objects=0#qt-science_center_objects
Other data sites

https://www.naturalearthdata.com/
https://data.humdata.org
https://data.humdata.org/dataset/wfp-geonode-ica-malawi-natural-shocks-risk
https://pmm.nasa.gov/data-access/downloads/gpm
https://disc.gsfc.nasa.gov/
https://developers.google.com/earth-engine/datasets/catalog/USGS_SRTMGL1_003
https://developers.google.com/earth-engine/datasets/catalog/CSP_ERGo_1_0_Global_ALOS_landforms
https://developers.google.com/earth-engine/datasets/catalog/CSP_ERGo_1_0_Global_SRTM_mTPI
https://soilgrids.org/
https://www.tamsat.org.uk/data/rfe/index.cgi
https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form
https://cds.climate.copernicus.eu/cdsapp#!/dataset/cems-glofas-historical?tab=overview
https://osm.org/copyright (only if you can find historical data before 2019)


#  https://zindi.africa/competitions/2030-vision-flood-prediction-in-malawi/data
