
#### Title: Census data figures
#### Purpose: Maps and figures for Census data
#### Author: David Coomes
#### Date: 9/11/2024

library(tigris)


# getting CA tract map
ca_zctas <- zctas("CA", year=2010, cb=TRUE)
