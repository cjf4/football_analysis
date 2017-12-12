##Win Probability and Win Probability Added as a Metric for NFL Performance

My goal for this project is to fit a classification model to NFL plays to derive a metric called “Win Probability”, and an additive metric “Win Probability Added”. My hypothesis is that WPA will be a better indicator of player performance than traditional player metrics.

### Data

Data was obtained using the [nfldb](https://github.com/BurntSushi/nfldb) postgres database and associated python package. The data collection scripts require a connection to an installed local copy of the nfldb databse. Directions for installation can be found [here](https://github.com/BurntSushi/nfldb/wiki/Installation).

Once downloaded, raw data was queried via raw SQL and transformed to an appropriate format for classification model fitting.

###Modelling