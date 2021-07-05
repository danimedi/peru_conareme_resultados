The data sets were created with a "tidy data" approach. Each row representing an observation for an individual in a given year, and each column representing a variable.

To distinguish those who got into a residency spot from those who did not check the columns related to 'ingresantes' (e.g., `Especialidad/SubEspecialidad_ingresantes`). Those who did not got into residency do not have this information (i.e., the rows contain `NA` for that columns).

`data_set_deidentified.csv` contains the final data but without the columns containing sensible information that could be used to identify the individuals.

`data_set.csv`contains the final data with all the information but this is not publicly available.

The variables (columns) available are:

- `year`: year.
- `Nombres`: first names
- `Apellido Paterno`: last name from father
- `Apellido Materno`: last name from mother
- `Especialidad/SubEspecialidad_postulantes` and `Especialidad/SubEspecialidad_ingresantes`: specialty/subspecialty during application and that in which the person got in, respectively
- `Universidad_postulantes` and `Universidad_ingresantes`: university
- `gender`: gender obtained from the first name; F = female, M = male
- `department_*`: department obtained from university
