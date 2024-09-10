# 2024 D4 HACK WEEK: DISASTERS, DEMOGRAPHY, DISPARITIES, AND DECISIONS
## Team: Claim to Flame (University of Washington & Columbia University)
#### Joan Casey, Vivian Do, Heather McBrien, David Coomes, Karen Chen, Lauren Wilner
#### September 9-13, 2024

This is a repository for UW to work on a project for CSDE/NOAA hosted D4 hack week at UW (September 2024). The goal of this workshop is to generate research products and methods for improving the spatiotemporal scales with which we can do the following: 
1. **Investigate the human behavior and societal adaptive responses** to, and impacts of, severe weather and climate-related events, particularly flooding associated with atmospheric rivers, hurricanes, and severe storms, but also including other extreme events such as heat or fire.
2. **Address the research gaps linking mitigation to adaptation and resilience in relation to severe weather.** This will involve exploring co-benefits for human well-being from climate adaptation strategies that will further contribute to resilience to extreme weather events and climate mitigation.
3. **Explore pathways to better understand the dynamics of decisions and population disparities** in responses to and impacts of past extreme climate / weather events.
*More information can be found here: https://csde.washington.edu/research/2024-d4-workshop/*

Specifically, this team will study the following: 

**Study years**: 2000-2023

**Study setting**: California

**Study questions**: What are the population characteristics of those actively applying for FEMA assistance for wildfire disasters? Within this group, what are the individual- (e.g., owner vs. rental status, level of disaster preparedness, reliance on electrical medical equipment) and area-level (e.g., neighborhood poverty) factors associated with successfully receiving FEMA aid or the amount of FEMA aid? Do the factors associated with successfully receiving FEMA aid vary by disaster type (i.e., wildfire disaster vs. wildfire disaster co-occurring with extreme heat)?

**Product**: Dataset for public use


We will do this using the following data sources: 
| Dataset | Description | URL | Spatial resolution (everything will be limited to CA) | Temporal resolution (including data range) | Unique ID |
|------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------|-------------------------------------------------------|---------------------------------------------|-----------------------|
| FEMA wildfire claims (from FEMA IHP dataset, including power outage variable) | FEMA applicant-level data for the Individuals and Households Program (IHP). The location is represented by county, city, and zip code. This dataset contains IA applications from DR1439 (declared in 2002) to those declared over 30 days ago. The full data set is refreshed on an annual basis; the last 18 months of data are refreshed weekly. This dataset includes all major disasters and includes only valid registrants (applied in a declared county, within the registration period, having damage due to the incident and damage within the incident period). | [Link](https://www.fema.gov/openfema-data-page/individuals-and-households-program-valid-registrations-v1) | ZIP code | 2002 - 2024 | Unique applicant ID |
| Wildfire data (CalFire) | Dataset from 2000-2020 containing all fire boundaries and burn dates for wildfires in California. New data are updated weekly so access to later years is possible. | [Link](https://www.fire.ca.gov/incidents) | Fire boundaries are given at high spatial resolution (<1km) | 2000-present | Fire IDs |
| Temperature data (PRISM) | Dataset containing temperature mins and maxes as well as humidity daily from 19something to present, from a high spatiotemporal resolution model of temperature (the PRISM model). | [Link](https://prism.oregonstate.edu/) | 1 km | 1987 - present | 1km grid squares |
| ACS and Census data | 2020 US Census data and 5-year ACS estimates at the zip code tabulation area (ZCTA). | [Link](https://www.census.gov/) | We plan on using ZCTA area indicators to align with our exposure and outcome. | We will match our exposure/outcome data: 2017-2023 | ZIP code |
| CalEnviroScreen | CalEnviroScreen is a measure of environmental injustice - it's a screening tool designed to identify communities burdened by multiple sources of pollution. | [Link](https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-40) | ZIP code | 2015-2023 | ZIP code |
| FEMA National Household Survey | The National Household Survey (NHS) tracks progress in personal disaster preparedness through investigation of the American public's preparedness actions, attitudes, and motivations. | [Link](https://www.fema.gov/about/openfema/data-sets/national-household-survey) | ZIP code | 2017-2023 | Unique respondent ID |
| EJI (if extended to additional states) (CDC) | The Environmental Justice Index (EJI) ranks the cumulative impacts of environmental injustice on health at the census tract in the US. | [Link](https://www.atsdr.cdc.gov/placeandhealth/eji/index.html) | Census tract | No date - created during 2020-2021, which is a fairly good match for our current project (2017-2023) | Census tract |


Our output dataset will contain the following columns: 
| Variable Name                          | Description                                                                                                      | Type        | Example Values                 | Dataset        |
|----------------------------------------|------------------------------------------------------------------------------------------------------------------|-------------|--------------------------------|----------------|
| year (PRIMARY KEY)                     | Year of the wildfire disaster or co-occurring disaster event                                                     | Numeric     | 2000, 2024                     | all            | 
| zcta (PRIMARY KEY)                     | ZIP code tabulation area for California                                                                          | String      | 06037 (Los Angeles County)     | all            |
| wf_history                             | Has this area experienced a wildfire within the last X years?                                                    | String      |                                | wf data        |
| fire_var2                              | TBD                                                                                                              | String      |                                | wf data        |
| fire_var3                              | TBD                                                                                                              | String      |                                | wf data        |
| fire_var4                              | TBD                                                                                                              | String      |                                | wf data        |
| applicant_id                           | Unique identifier for each FEMA applicant                                                                        | String      | A123456789                     | fema           |
| disaster_number                        | Unique identifier for each FEMA declared disaster                                                                | String      | 123456789                      | fema           |
| disaster_type                          | Type of disaster event (e.g., wildfire, wildfire with extreme heat)                                              | String      | "Wildfire", "Wildfire + Heat"  | fema           |
| owner_vs_renter                        | Housing status of the applicant                                                                                  | Categorical | "Owner", "Renter"              | fema           |
| insurance_coverage                     | Type of insurance coverage the applicant has (e.g., homeowners, renters)                                         | Categorical |                                | fema           |
| medical_equipment_dependence           | Indicator if the applicant relies on electrical medical equipment                                                | Boolean     | True, False                    | fema           |
| fema_assistance_application_status     | Status of FEMA assistance application (e.g., approved, denied)                                                   | Categorical | "Approved", "Denied"           | fema           |
| fema_aid_amount                        | Amount of FEMA aid received by the applicant                                                                     | Numeric     |                                | fema           |
| disaster_preparedness_level            | Self-reported level of disaster preparedness                                                                     | Categorical | "Low", "Moderate", "High"      | fema_survey    |
| co_occurring_disaster_indicator  | Indicator for whether the wildfire disaster co-occurred with another extreme event (e.g., extreme heat) (using PRISM??)| Boolean     | True, False                    | prism          |
| household_ages                         | Ages of household members                                                                                        | Numeric     |                                | census/acs     |
| household_income                       | Household income of the applicant                                                                                | Numeric     |                                | census/acs     |
| health_insurance                       | area-level health insurance coverage level                                                                       | Numeric     |                                | census/acs     |
| education                              | area-level education                                                                                             | Numeric     |                                | census/acs     |
| poverty                                | area-level poverty                                                                                               | Categorical |                                | census/acs     |
| race_ethnicity                         | area-level race/ethnicity                                                                                        | Categorical |                                | census/acs     |
| migration (name tbd)                   | area-level migration                                                                                             | Categorical |                                | census/acs     |
| linguistic isolation                   | area-level linguistic isolation                                                                                  | Categorical |                                | census/acs     |
| median income (area level)             | area-level income                                                                                                | Numeric     |                                | census/acs     |
| household_size                         | Number of people in the applicant's household                                                                    | Numeric     |                                | census/acs     |
| registered_voter_perc                  | Percent registered voters                                                                                        | Numeric     |                                | joan has data? |
|----------------------------------------|------------------------------------------------------------------------------------------------------------------|-------------|--------------------------------|----------------|



