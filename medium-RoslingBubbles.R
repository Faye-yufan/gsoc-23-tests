library(animint2)
library(dplyr)

world.bank <-
  # pull the country data down from the World Bank - three indicators
  wbstats::wb_data(
    indicator = c("SP.DYN.LE00.IN", "NY.GDP.PCAP.CD", "SP.POP.TOTL"), 
    country = "countries_only", 
    start_date = 1960, 
    end_date = 2022
  ) |> 
  # pull down mapping of countries to regions and join   
  dplyr::left_join(
    wbstats::wb_countries()  |>  
      dplyr::select(iso3c, region)
  ) 
world.bank$date <- as.numeric(world.bank$date)

bubble <- ggplot(world.bank) +
  geom_text(
    aes(x = 7.5, y = 5, label = date, key = date), 
    showSelected = "date",
    size = 100, 
    alpha = 0.2,
    color = 'black', 
    family = 'Oswald'
  ) + 
  geom_point(aes(x = log(NY.GDP.PCAP.CD), 
                 y = SP.DYN.LE00.IN, 
                 size = SP.POP.TOTL,
                 fill = region, 
                 key = country),
             clickSelects = "country",
             showSelected = "date",
             colour = "black",
             colour_off = "transparent",
             stroke = 2,
             alpha = 0.8,
             alpha_off = 0.3,
             shape = 21) + 
  scale_fill_brewer(palette = "Set2") +
  scale_size(range = c(1, 70)) +
  theme(legend.position = "none") +
  xlab("Log GDP per capita") +
  ylab("Life expectancy") 

select.year <- ggplot(data = world.bank) +
  geom_tallrect(aes(
    xmin = date - 1 / 2, xmax = date + 1 / 2),
    clickSelects = "date",
    alpha = 0.1)+
  geom_line(aes(
    date, SP.DYN.LE00.IN, group=country, colour=region),
    clickSelects="country",
    size=4, alpha=3/5) +
  xlab("Year") +
  ylab("Life expectancy")

viz <- animint(bubble, select.year,
               time = list(variable = "date", ms = 2000),
               duration = list(date = 500),
               first=list(date=1960, country=c("United States", "China")),
               selector.types = list(country = "multiple"))

viz
