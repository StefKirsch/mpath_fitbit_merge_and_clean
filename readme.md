# m-path and fitbit data merging and cleaning

This script takes the data downloaded from

-   the m-path server and
-   fitabase

The data is merged transferred into the desired tidy format. Some base statistics (mean per day, minute and hour) are computed as well, some of them in the time window before the ESM beeps

## Requirements

1.  To run the script you need [Rstudio](https://posit.co/download/rstudio-desktop/).
2.  You need to have all the libraries installed. Rstudio should prompt you to install all necessary libraries automatically
3.  Make sure that `translation_key.xlsx` is located in the project root folder.

```{=html}
<!-- -->
```
4.  The script will prompt you yo manually specify
    -   The path of the folder where the ESM files (from m-path) are located. Make sure that all files are placed directly in that folder and not in subfolders

    -   The folder where the fitbit data is located (the files can be in subfolders)

    -   The output folder for the resulting file

## Notes

Execution of this script can take a while. Make sure to wait until you get the message

```         
Script completed!
```

in the console.

I tested the script with the sample data I received and also tested it with incomplete data sets. It seems reasonably robust to those. There might be some edge cases though where the script breaks down.

In cases data is missing, in either the fitbit or the ESM files, the corresponding rows will not exists in the final file. This means that no all beeps will be there for that day. So far I did not check explicitly what happens if data is missing only in the ESM data, but not in the fitbit data, or vice-versa.

> Tip: You can collapse the sections below in R and jump to a section with with the navigation pane on the right (you might have to make it visible)

Make sure that all variables in the the ESM data are also in `translation_key.xlsx`. If that is not the case, the missing variables will keep their Dutch names and you will get a warning.

Please note that the names of the factor levels are not being translated at this point.
