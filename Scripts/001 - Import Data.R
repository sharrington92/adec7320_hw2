# Get data
{
  file.vaccines <- "COVID-19_Vaccinations_in_the_United_States_County.csv"
  vaccinations.raw <- read_csv(file.path(folder.data.raw, file.vaccines))
  
  file.covid <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  covid.raw <- read_csv(file.covid)
  
  file.demographics <- "cc-est2019-alldata.csv"
  demographics.raw <- read_csv(file.path(folder.data.raw, file.demographics))
}

