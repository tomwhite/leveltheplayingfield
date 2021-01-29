# Level The Playing Field

Analysis of school funding in Wales http://leveltheplayingfield.wales/

## Data sources

The majority of the data is from [Stats Wales](https://statswales.gov.wales/).

CSV files in the _data_ directory were downloaded from the Stats Wales website, and are named
after the final part of the Stats Wales URL.

* https://statswales.gov.wales/Catalogue/Population-and-Migration/Population/Estimates/Local-Authority/populationestimates-by-localauthority-year
    * _Note: Make sure Wales is the only expanded country. Click on Year dropdown, and Show All to get all years_. Also one export per age group too.
* https://statswales.gov.wales/Catalogue/Local-Government/Finance/Revenue/Delegated-School-Budgets/delegatedschoolbudgetsperpupil-by-sector (Individual schools budget per pupil, and Number of pupils)
* https://statswales.gov.wales/Catalogue/Local-Government/Finance/Revenue/Delegated-School-Outturn/levelofreservescarriedforward-by-sector

### Address data

Data in the _data/geo_ directory is from https://gweddill.gov.wales/statistics-and-research/address-list-of-schools/?lang=en.
Postcode to lat/long conversion was carried out using https://www.doogal.co.uk/BatchGeocoding.php.

### School-level data

Data in the _data/sheets_ directory is data that was retrieved via FOI requests (see the website for
more information) and combined with data from the Stats Wales site to give a per-school view.

This data is stored in Google Sheets, but a local copy is provided in _data/sheets_ to make
it possible to reproduce all the analyses.

Starting from 2019, the school support category data was downloaded from [gov.wales](https://gov.wales/national-school-categorisation-system-support-categories?_ga=2.207195155.1087568766.1580471338-638608250.1543144354)
in XLSX format and saved as CSV files in the _data/support-category_ directory.

## Running

In RStudio, run _batch.R_ to generate all the reports in the _docs_ directory. These are hosted on the main website at
http://leveltheplayingfield.wales.

There are custom analyses in the _posts_ directory for generating reports for the blog posts. These are written to the
_docs/posts_ directory.

There are experiments and _ad hoc_ analyses in _adhoc.R_. These do not create output for the website.

If you wish to run your own analyses, then run _load_data.R_ in RStudio, which will load all the data
into several dataframes (`all_schools`, `population_with_age`, etc).
