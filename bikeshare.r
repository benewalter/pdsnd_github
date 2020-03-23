
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

# Loading ggplot2 package to create visualizations with it
library(ggplot2)

# Solution code: Creating a boxplot to look at the trip durations for subscribers and customers, using ggplot

# User.Type is used as the x-variable and Trip.Duration in minutes (therefore divided by 60) as the y-variable
# As some observations for User.Type are NAs, they are removed by subsetting the ny data
ggplot(aes(x=User.Type, y = Trip.Duration/60), data = subset(ny, User.Type == 'Customer' | User.Type  == 'Subscriber')) +
    # Selection of a boxplot and colours
    geom_boxplot(color = 'black', fill = '#099DD9') +
    # Zooming in on the data to have a better depiction of the data, while not displaying only a few (extreme) outliers
    coord_cartesian(ylim = c(0,90)) + 
    # Adaptation of the breaks of the y-axis
    scale_y_continuous(breaks = seq(0, 90, 5)) +
    # Labeling of the x- and y-axis as well as adding a title to the plot
    xlab('User Type') +
    ylab('Trip Duration in Minutes') +
    ggtitle('\t\tDistribution of Trip Durations -- Subscribers and  Customers') +
    # Adaptation of the size of the plot title
    theme(plot.title = element_text(size=16))

# Looking at the summary statistics for the variable Trip.Duration (in minutes), distinguishing between Customers and Subscribers
by(ny$Trip.Duration/60, ny$User.Type, summary)

# To identify the observations in the combined data set, we first have to create an additional column that specifies for each observation the city.

# Creation of a vector that contains the names of the three cities
city_names = c('New York', 'Washington', 'Chicago')
# Creation of a function that adds the corresponding city name to each observation
add_city_name = function(city, city_names) {
    city$city_name = city_names
    return(city)
}

#I did not use a function or loop in this case since copy and pasting was easy without much effort here.

# Execution of the above-defined function: Adding the corresponding city names to the observations
ny = add_city_name(ny, city_names[1])
wash = add_city_name(wash, city_names[2])
chi = add_city_name(chi, city_names[3])

# To combine the three data sets, the two columns that are only contained in two of the data sets are removed.
ny = ny[, c(1,2,3,4,5,6,7,10)]
wash = wash[, c(1,2,3,4,5,6,7, 8)]
chi = chi[, c(1,2,3,4,5,6,7, 10)]

# The three prepared data sets are combined to one data set 'all_cites'
all_cities = rbind(ny, wash, chi)

# Creation of histograms that represent the distribution of the trip durations (in minutes), with a facet based on the city

ggplot(data = all_cities, aes(x = Trip.Duration/60)) +
    # Selection of a histogram as the type of visualizations and definition of the colours
    geom_histogram(binwidth = 6, color = 'black', fill = '#099DD9') +
    # Zooming in on the data for a better visualization
    coord_cartesian(xlim = c(0,120)) +
    # Creation of a facet based on the three cities
    facet_wrap(~city_name)+
    # Definition of the axis labels as well as the title of the plot
    xlab('Trip Duration [Minutes]')+
    ylab('Number of Trips') +
    ggtitle('\t\tDistribution of Trip Durations -- Comparison of the Cities') +
    # Setting the breaks
    scale_y_continuous(breaks = seq(0,30000, 1000)) +
    scale_x_continuous(breaks = seq(0,120, 15))


# Looking at the number of trips made in the three cities. To do this, we count the number of rows for each data frame.
nrow(ny)
nrow(wash)
nrow(chi)

# Looking at the summary statistics of the trip duration (in minutes) for each of the three cities.
by(all_cities$Trip.Duration/60, all_cities$city_name, summary)

# We first add a column to the all_cities data set that contains for each trip the day of week.

# First, we duplicate the all_cities data set to have a backup for testing if something goes wrong.
all_cities_copy = all_cities
# We change the Start.Time column to only include the date without the time.
all_cities_copy$Start.Time = substr(all_cities_copy$Start.Time, 1, 10)
# We transform the data type of the Start.Time column from factor to date to determine the day of week.
all_cities_copy$Start.Time = as.Date(all_cities_copy$Start.Time, format = "%Y-%m-%d")
# We use the function strftime to determine the day of week for each observation based on the date.
all_cities_copy$Day.of.Week = strftime(all_cities_copy$Start.Time,'%A')

# Ordering of the observations (Week.of.Days) to display them in order from Monday to Sunday
all_cities_copy$Day.of.Week <- ordered(all_cities_copy$Day.of.Week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
"Friday", "Saturday", "Sunday"))

# Solution code: Creating a boxplot with facets to look at distributions of the trip durations across weekdays and the cities.

# User.Type is used as the x-variable and Trip.Duration in minutes (therefore divided by 60) as the y-variable
# As some observations for User.Type are NAs, they are removed by subsetting the ny data
ggplot(aes(x=Day.of.Week, y = Trip.Duration/60), data = subset(all_cities_copy, !is.na(Day.of.Week))) +
    # Selection of a boxplot and colours
    geom_boxplot(color = 'black', fill = '#099DD9') +
    # Zooming in on the data to have a better depiction of the data
    coord_cartesian(ylim = c(0,60)) + 
    # Creation of a facet based on the three cities
    facet_wrap(~city_name)+
    # Adaptation of the breaks of the y-axis
    scale_y_continuous(breaks = seq(0, 60, 5)) +
    # Labeling of the x- and y-axis as well as adding a title to the plot
    xlab('Day of Week') +
    ylab('Trip Duration in Minutes') +
    ggtitle('\t\t\t\tDistribution of Trip Durations -- Day of Week') +
    # Adaptation of the size of the plot title
    theme(plot.title = element_text(size=12)) +
    theme(axis.text.x = element_text(angle = 90))

# Summary statistics that reveal for each city for each weekday relevant figures for the trip duration.
by(all_cities_copy$Trip.Duration/60, list(all_cities_copy$Day.of.Week, all_cities_copy$city_name), summary)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
