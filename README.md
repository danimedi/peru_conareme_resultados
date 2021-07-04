# peru_conareme_resultados

> Zenodo link: https://zenodo.org/record/5068770

## About the data

This repository contains the data, and the code used to obtain and clean this data, of the people who applied and those who got residency (medical specialty) spots during the different processes during the last years.

The results were obtained from the [official web site of the "Consejo Nacional de Residentado Médico" (CONAREME)](https://www.conareme.org.pe/web/), which is the organization in charge of the process. The web site records the results for different years, since 2009, but important variables are missing (e.g., incomplete description of specialties and missing data for specialties), since 2013 the data is complete enough, but it only registers the information of those who applied without information about who got into a specialty. Since 2016 the information for both those who applied and those who got the specialty is available.

In 2014 and 2015 there was an "extraordinary" exam, the results for these exams were not considered because they were not present in most of the years.

Information for the year 2018 was not available through links within the page, but it was found using this link in the same web domain: https://www.conareme.org.pe/sigesin/intranet/docCN/documentos/22-07-2016/_Relacion_Final_de_Postulantes_30-05-2018%2017_39_58.pdf

## Organization

- `data` contains the final data.
- `data_raw` is a folder containing raw data, some of this data is generated by the code, so it is not commited (i.e., PDF and CSV files).
- `renv` contains information about the R packages used.
- `src` contains the code used to create the final data. Inside this folder there is the file `runall.R`, this file is used to run all the code needed to generate the final data (however, access to the Google Drive folder containing the PDF files is necessary).

## About the code

The code is written in R language and the explanation of the code can be found within the files, the file `runall.R` within `src` is a good place to start to understand the pipeline used.

> The function `renv::restore()` can be used to restore the packages used from `renv.lock`.
