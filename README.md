# Level The Playing Field

Analysis of school funding in Wales http://leveltheplayingfield.wales/

Due to Covid-19, some data sources were not updated in 2020 (see notes below). Therefore some charts and tables cannot be updated for the latest school year.

## Data sources

The majority of the data is from [Stats Wales](https://statswales.gov.wales/).

CSV files in the _data_ directory were downloaded from the Stats Wales website, and are named
after the final part of the Stats Wales URL.

- https://statswales.gov.wales/Catalogue/Population-and-Migration/Population/Estimates/Local-Authority/populationestimates-by-localauthority-year
  - _Note: Make sure Wales is the only expanded country. Click on Year dropdown, and Show All to get all years_. Also one export per age group too.
- https://statswales.gov.wales/Catalogue/Local-Government/Finance/Revenue/Delegated-School-Budgets/delegatedschoolbudgetsperpupil-by-sector (Individual schools budget per pupil, and Number of pupils)
- https://statswales.gov.wales/Catalogue/Local-Government/Finance/Revenue/Delegated-School-Outturn/levelofreservescarriedforward-by-sector
- https://statswales.gov.wales/Catalogue/Local-Government/Finance/Revenue/Delegated-School-Budgets/delegatedschoolbudgetsperpupil-by-school (Individual schools budget, Individual schools budget per pupil, and Number of pupils)
  - _Note: Make sure Caerphilly Middle schools are expanded._
  - _From 2020-21. This data was previously entered into Google Sheets_
- https://statswales.gov.wales/Catalogue/Local-Government/Finance/Revenue/Delegated-School-Outturn/levelofreservescarriedforward-by-school
  - _Note: Make sure Middle schools are expanded._
  - _From 2020-21. This data was previously entered into Google Sheets_

### Address data

Data in the _data/geo_ directory is from https://gov.wales/address-list-schools.
Download the latest ODS file, and export the "Maintained" sheet to CSV (using OpenOffice).
Postcode to lat/long conversion was carried out using https://www.doogal.co.uk/BatchGeocoding.php.

### School-level data

Data in the _data/sheets_ directory is data that was retrieved via FOI requests (see the website for
more information) and combined with data from the Stats Wales site to give a per-school view.

This data is stored in Google Sheets, but a local copy is provided in _data/sheets_ to make
it possible to reproduce all the analyses.

Starting from 2019, the school support category data was downloaded from [gov.wales](https://gov.wales/national-school-categorisation-system-support-categories?_ga=2.207195155.1087568766.1580471338-638608250.1543144354)
in XLSX format and saved as CSV files in the _data/support-category_ directory.

- _Not updated in 2020, and will not be assessed for 5 years._

## Running

In RStudio, run _batch.R_ to generate all the reports in the _docs_ directory. These are hosted on the main website at
http://leveltheplayingfield.wales.

There are custom analyses in the _posts_ directory for generating reports for the blog posts. These are written to the
_docs/posts_ directory.

There are experiments and _ad hoc_ analyses in _adhoc.R_. These do not create output for the website.

If you wish to run your own analyses, then run _load_data.R_ in RStudio, which will load all the data
into several dataframes (`all_schools`, `population_with_age`, etc).

## Annual update

To update the reports every year:

- Download new data as listed in "Data Sources" above.
- Download the new list of schools. (TODO: document how to update school list - see _qa.R_)
- Update the "Delegation rates %" spreadsheet in Google Sheets.
- Add FSM numbers from My Local School to the spreadsheets in Google Sheets. (This data is not available on Stats Wales)
- Run _download_data.R_.
- Run _batch.R_.
- Run `python3 make_site.py`, and compare the local site to the live one.
- Upload the new site.
