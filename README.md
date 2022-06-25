# Shiny App for Viewing Labor Force Data

Currently set up to view data by one or more counties, specifically the counties in Eastern Iowa where I'm located. Code could be easily modified for different counties and their respective FIPS code. Although right now to change the geo, you'll first need to set the BLS URL to your desired state (right now it's coded for only single state functionality) and then you need to swap out the county names in their respective location and the pre-selected counties.

Future plans
- add more functions/variables to clean up code and eliminate duplication
- make it possible to pass a dataframe of county FIPS codes and corresponding names to set the geos

###### Data from the BLS Local Area Unemployment Survey.
