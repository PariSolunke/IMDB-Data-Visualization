We used R to filter data and create a csv file, we used grep to filter the data, gsub to remove unwanted details present in the cell and duplicated to remove cell with similar identifier (movie name, year). We have removed rows that contains entries from the databases as per the conditions mentioned on the evl website. We have also kept track of certificates (G, PG, etc.) in the US that has changed a bit over the years and concatenated all the keywords for a movie in a single cell.

We prepared a R script that takes all (seven) files as input and generates a CSV file combining all of them. To run the script you have to use this command “Rscript imdb.R”.

The files required can be found at ftp://ftp.fu-berlin.de/pub/misc/movies/database/frozendata/

The files used for this project are:
1)release-dates.list
2)running-times.list
3)certificates.list
4)genres.list
5)keywords.list
6)movies.list
7)ratings.list
