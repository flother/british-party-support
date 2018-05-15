#' A script to download British parlimentary elections data from Electoral
#' Calculus for elections between 1955 and 2017 inclusive, and plot Conservative
#' and Labour vote share as a ridgeline plot.
library(tidyverse)  # v1.2.1
library(ggridges)   # v0.5.0


#' Read a single election's data from a delimited file on the Electoral Calculus
#' website.
#'
#' @param election Unique identifier for an election
#' @return A data frame containing party vote shares.
#' @examples
#' read_election_data(1955)
#' read_election_data("1974feb")
read_election_data <- function(election) {
  url <- paste0("http://www.electoralcalculus.co.uk/electdata_", election, ".txt")
  if (election == "1974feb") {
    label <- "Feb 1974"
  } else if (election == "1974oct") {
    label <-"Oct 1974"
  } else {
    label <- str_sub(election, 1, 4)
  }
  read_delim(url, delim = ";") %>%
    mutate(election = label)
}

#' Retrieve all data for elections.
#'
#' @return A data frame containing party vote shares for each election.
#' @example read_elections_data()
read_elections_data <- function() {
  bind_rows(lapply(c("1955", "1959", "1964", "1966", "1970", "1974feb",
                     "1974oct", "1979", "1983", "1987", "1992ob", "1997",
                     "2001ob", "2005ob", "2010", "2015", "2017"),
                   read_election_data))
}

#' Retrieve tidy data for Conservative and Labour vote share for all elections.
#'
#' @return A tidy data frame with three columns: election data, party, and vote.
#' @example read_con_lab_vote_share()
read_con_lab_vote_share <- function() {
  read_elections_data() %>%
    # Remove Northern Ireland; Labour isn't a registered political party there,
    # and the Conservatives have only recently fielded candidates.
    filter(Area != 1) %>%
    mutate(election = fct_rev(fct_inorder(election)),
           Conservative = CON / Electorate,
           Labour = LAB / Electorate) %>%
    select(election,
           Conservative,
           Labour) %>%
    gather(-election,
           key = "party",
           value = "vote")
}

# Download the data and plot it.
ggplot(data = read_con_lab_vote_share(),
       mapping = aes(vote, election, fill = party)) +
  geom_density_ridges(colour = NA) +
  scale_x_continuous(breaks = c(0, .25, .50, .75, 1),
                     labels = scales::percent) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(name = "party",
                    values = alpha(c("#0087dc", "#dc241f"), .5)) +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "Support for the large political parties",
       subtitle = "Distribution of constituency votes in British general elections",
       caption = "Source: Electoral Calculus",
       x = NULL,
       y = NULL) +
  theme_ridges() +
  theme(legend.position = "top")

# Save the plot as a PNG in the working directory.
ggsave('tory-labour-1955-2017.png',
       width = unit(9, "cm"),
       height = unit(10, "cm"),
       dpi = "print")
