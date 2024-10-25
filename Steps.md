

Step 1: Webscraping and cleaning raw data from IMSLP

Webscraping: the scraping code is in 2.py and 3.py, a first-stage cleaning after immediate download is in clean_code.ipynb. We then have imslp_1.xlsx.

This file contains 35 composers' personal information and their compositions information.

For the other 3 composers, which are Dmitri Shostakovich, Benjamin Britten, and Arthur Bliss, I had to manually search for their info on the internet 
then manually organize and integrate them into my existent database. This is because their work are still under the protection of copyright in both the US and Canada,
so the IMSLP website does not have a complete set the scores of their compositions, and since IMSLP is a website built for music scores, when nobody upload the scores, the pages
do not contain any info related to them. The three files are respectively Shostakovich.xlsx, Britten.xlsx, and Bliss.xlsx and I integrated them to imslp_1.xlsx.


After scraping down the raw files from the IMSLP website, I started data cleaning.

1. The original datafile is imslp_1.xlsx


Step 2: API and cleaning raw data from Spotify


Step 3: Requesting and cleaning raw data from orchestras and symphonies worldwide


Step 4: Regression and adjustments
