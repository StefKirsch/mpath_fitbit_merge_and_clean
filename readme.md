# m-path and fitbit data merging and cleaning

This script takes the data downloaded from

-   the m-path server and
-   fitabase

The data is merged transferred into the desired tidy format. Some base statistics (mean per day, minute and hour) are computed as well, some of them in the time window before the ESM beeps

## Requirements

1.  To run the script you need [Rstudio](https://posit.co/download/rstudio-desktop/).
2.  You need to have all the libraries installed. Rstudio should prompt you to install all necessary libraries automatically
3.  The following folder structure is recommended. Make sure that `translation_key.xlsx` is located in the project root folder.

``` bash
project root
|--input
|--|--esm
|--|--|--ESM_006_week1.xlsx
|--|--|--ESM_006_week2.xlsx
|--|--|-- ...
|--|--fitbit
|--|--|--week_1 # folder where all fitbit raw data of one batch should be located
|--|--|--|--ID_heartrate_1_min_.csv # this file is unused
|--|--|--|--ID_heartrate_seconds_.csv
|--|--|--|--ID_minuteStepsNarrow_.csv
|--|--|--week_2
|--|--|--|-- ...
|--output
|--main.R
|--translation_key.xlsx
```

4.  The script will prompt you yo manually specify
    -   The path for the ESM file (from m-path)

    -   The folder where the fitbit data is located

    -   The output folder

So in that regard you can diverge from the folder structure shown above, if needed. Make sure to not have several fitbit files with the keywords `heartrate_seconds` or `minuteStepsNarrow` in one folder. In that case you will get a warning.

## Notes

Execution of this script can take a while. Make sure to wait until you get the message "done" in the console.

I tested the script with the sample data I received and also tested it with incomplete data sets. It seems reasonably robust to those. There might be some edge cases though where the script breaks down.

> Tip: You can collapse the sections below in R and jump to a section with with the navigation pane on the right (you might have to make it visible)

Make sure that all variables in the the ESM data are also in `translation_key.xlsx`. If that is not the case, the missing variables will keep their Dutch names and you will get a warning.
