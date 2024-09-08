#' @title Movie information and user ratings from IMDB.
#' @name movies_long
#' @details Modified dataset from `{ggplot2movies}` package.
#'
#' @format A data frame with 1,579 rows and 8 variables
#'
#'   - title.  Title of the movie.
#'   - year.  Year of release.
#'   - budget.  Total budget (if known) in US dollars
#'   - length.  Length in minutes.
#'   - rating.  Average IMDB user rating.
#'   - votes.  Number of IMDB users who rated this movie.
#'   - mpaa.  MPAA rating.
#'   - genre. Different genres of movies (action, animation, comedy, drama,
#'     documentary, romance, short).
#'
#' @source <https://CRAN.R-project.org/package=ggplot2movies>
#'
#' @examples
#' dim(movies_long)
#' head(movies_long)
#' dplyr::glimpse(movies_long)
"movies_long"


#' @title Edgar Anderson's Iris Data in long format.
#' @name iris_long
#' @details This famous (Fisher's or Anderson's) iris data set gives the
#'   measurements in centimeters of the variables sepal length and width and
#'   petal length and width, respectively, for 50 flowers from each of 3 species
#'   of iris. The species are Iris setosa, versicolor, and virginica.
#'
#' This is a modified dataset from `{datasets}` package.
#'
#' @format A data frame with 600 rows and 5 variables
#'
#'   - id. Dummy identity number for each flower (150 flowers in total).
#'   - Species.	The species are *Iris setosa*, *versicolor*, and
#'     *virginica*.
#'   - condition. Factor giving a detailed description of the attribute
#'     (Four levels: `"Petal.Length"`, `"Petal.Width"`,  `"Sepal.Length"`,
#'     `"Sepal.Width"`).
#'   - attribute.	What attribute is being measured (`"Sepal"` or `"Pepal"`).
#'   - measure.	What aspect of the attribute is being measured (`"Length"`
#'     or `"Width"`).
#'   - value.	Value of the measurement.
#'
#' @examples
#' dim(iris_long)
#' head(iris_long)
#' dplyr::glimpse(iris_long)
"iris_long"

#' @title Tidy version of the "Bugs" dataset.
#' @name bugs_long
#' @details This data set, "Bugs", provides the extent to which men and women
#'   want to kill arthropods that vary in freighteningness (low, high) and
#'   disgustingness (low, high). Each participant rates their attitudes towards
#'   all anthropods. Subset of the data reported by Ryan et al. (2013).
#'
#' @format A data frame with 372 rows and 6 variables
#'
#'   - subject. Dummy identity number for each participant.
#'   - gender. Participant's gender (Female, Male).
#'   - region. Region of the world the participant was from.
#'   - education. Level of education.
#'   - condition. Condition of the experiment the participant gave rating
#'     for (**LDLF**: low freighteningness and low disgustingness; **LFHD**: low
#'     freighteningness and high disgustingness; **HFHD**: high freighteningness
#'     and low disgustingness; **HFHD**: high freighteningness and high
#'     disgustingness).
#'   - desire. The desire to kill an arthropod was indicated on a scale from
#'     0 to 10.
#'
#' @references
#'
#' Ryan, R. S., Wilde, M., & Crist, S. (2013). Compared to a small, supervised
#' lab experiment, a large, unsupervised web-based experiment on a previously
#' unknown effect has benefits that outweigh its potential costs. *Computers in
#' Human Behavior*, *29*(4), 1295-1301.
#'
#' @examples
#' dim(bugs_long)
#' head(bugs_long)
#' dplyr::glimpse(bugs_long)
"bugs_long"
