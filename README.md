# Hispanic Health and Race Analysis 2016 and 2023

This project uses data from the Behavioral Risk Factor Surveillance System (BRFSS) to measure the impact of recent changes on racial classification by the Office of Management and Budget (OMB) on racial distribution and health outcomes in Puerto Rico, New York, and Florida.

## Repository content:

The repository includes files, code, and outputs for the analysis.

-   `data` is a folder containing raw data files from BRFSS for the years 2016 and 2023.
    -   The folder is currently empty as the files are too large to be uploaded to GitHub. Users can download the data from the CDC BRFSS website (<https://www.cdc.gov/brfss/annual_data/annual_data.htm>).
-   `script` is a folder containing two R markdown files with code:
    -   The first script reads in the data and performs basic data cleaning and wrangling instructions. Additionally, it generates a series of new variables for our analysis. It also saves the processed data as an RDS file to ease analysis later.
    -   The second script performs a series of analysis including: calculating totals by race and health outcomes, calculating means and confidence intervals, and performs t-test analysis for each state by race and ethnicity for all health outcomes between years.
-   `outputs` is a folder where results from the analysis are stored in.

## How do you run the code?

The code shared in this repository is structured to analyze data from the CDC BRFSS for the years 2016 and 2023 for three states: FL, NY, and PR.

To reproduce the code, users should first clone the repository to their local computers and download the data from the BRFSS into the `data` folder and unzip the data.

Once the data is downloaded and stored in the `data` folder, users should run the code `01_data_wrangling.Rmd`.

Required libraries and packages are installed into the local computer using the `p_load()` function in the {packman} package.

If the user is interested in analyzing a different set of states, these can be specified in the "State Parameter Setup" chunk by including the desired state FIPS codes into the `target_fips` vector.

Our analysis focuses on a set of desired variables, which are filtered for when the data is being read into the program. If the user is interested in a different set of variables, they should indicate this when data is being read in. A different set of variables will require also changing the parameters for labeling, converting variables to factor, and setting up the `health_vars` vector containing desired variables through which our functions will cycle through.

Running `02_data_analysis.Rmd` will output several .xlxs files containing results from our total estimates and statistical analysis. Results are exported in this manner because they facilitated usability for the research team. Results from our analysis are already included in the outputs folder and can be used to visualize and analyze results.

# NOTE

Google Gemini was used to refactor the code used for the analysis by helping create helper functions that calculated and performed statistical analysis and reducing repetitive code.
