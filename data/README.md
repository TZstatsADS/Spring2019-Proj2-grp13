# Project 2: Shiny App Development Version 2.0

### Data folder

The data directory contains data used in the analysis. This is treated as read only; in paricular the R/python files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.

**Fares Data**:

Link: https://www.transtats.bts.gov/AverageFare/

Description: original data covers the 1,000 largest city-pair markets in the 48 contiguous states. For each of the 1,000 largest city-pair markets, Table 1 lists the number of one-way passenger trips per day, the nonstop distance, the average market fare, and identifies the airlines with the largest market share and the lowest average fare; market share and average fares are provided for both airlines. Average fares are average prices paid by all fare paying passengers. 

**On-time Performance**: 
Link: https://transtats.bts.gov/ONTIME/

Description: original dataset includes departure and arrival statistics (scheduled departure time, actual departure time, scheduled elapse time, departure delay, wheels-off time and taxi-out time) by airport and airline; airborne time, cancellation and diversion by airport and airline. We used quaterly collected data during Year 2018. 

**Accident Data**:

Link: https://www.asias.faa.gov/apex

Description: Original dataset includes accident data from FAA Accident and Incident Data System(AIDS). Covers 102,946 records 
of accidents from 1978 to 2018. For each reords, the dataset includes details such as Event.Local.Date, Event.State, Damage Levels and so on. In this project, the processed dataset focus on the following aspects: Time(include year and month), State, Operator, Aircraft Maker, Damage level and Flight Type.






