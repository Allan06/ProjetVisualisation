setwd("C:/Users/Allan/Desktop/R/ProjetVisualisation")

library(tidyverse)
library(scales)
library(sf)
plot(1:100, (1:100) ^ 2, main = "plot(1:100, (1:100) ^ 2)")



" ============================================================================
                            Data Manipulation
  ============================================================================ "

"The Data : P10
 ============================================================================= "
round_1 <- read_csv('data/results_pres_elections_dept_2017_round_1.csv')
round_2 <- read_csv('data/results_pres_elections_dept_2017_round_2.csv')

"Parsed with column specification:
cols(
  dept_name = col_character(),
  muni_code = col_double(),
  dept_code = col_double(),
  muni_name = col_character(),
  registered_voters = col_double(),
  absent_voters = col_double(),
  present_voters = col_double(),
  blank_ballot = col_double(),
  null_balot = col_double(),
  votes_cast = col_double(),
  name = col_character(),
  firstname = col_character(),
  gender = col_character(),
  votes = col_double()
)"


"Data format : tibble : P12
 ============================================================================= "
class(round_2)
round_2$region_name


"dplyr::select() : P14-17
 ============================================================================= "
round_2 %>% select(region_name, 'LE PEN', MACRON)
round_2 %>% select(1:5)
round_2 %>% select(-c(3:7), -region_code)
round_2 %>% select(contains("vote"))


"dplyr::filter() : P18-20
 ============================================================================= "
round_2 %>% filter(region_name == "Provence-Alpes-Côte d'Azur")
round_2 %>% filter(registered_voters > 100000 & present_voters > 100000)


"dplyr::arrange() : P21
 ============================================================================= "
round_2 %>%
  arrange(registered_voters)


"dplyr::mutate() : P23-24
 ============================================================================= "
round_2 %>%
  mutate(voting_rate = present_voters/registered_voters) %>%
  select(c(1:4), voting_rate, everything())

round_2 %>%
  mutate(rank = min_rank(desc(MACRON))) %>%
  select(dept_name, MACRON, rank) %>%
  arrange(rank)


"dplyr::summarise() : P26
 ============================================================================= "
round_2 %>%
  summarise(total_votes = sum(votes_cast))

round_2 %>% group_by(region_name) %>%
  summarise(total_votes = sum(votes_cast))


"dplyr::left_join() : P27
 ============================================================================= "

geo_data <- read_csv("data/coordinates_regions_2016.csv")

round_2 %>% left_join(geo_data, by=c("region_code"="insee_reg")) %>%
  select(region_code, region_name, latitude, longitude, everything())


"dplyr::bind_rows() : P28
 ============================================================================= "
results <- round_1 %>% mutate(round = "Round 1") %>%
  bind_rows(round_2 %>% mutate(round = "Round 2"))

results

"tidyr::gather() : P30-32
 ============================================================================= "
round_2_bis <- round_2 %>% gather(candidate, votes, c(`LE PEN`, MACRON)) %>%
  arrange(region_name, dept_name) %>%
  select(region_name, candidate, votes, everything())

round_2 %>% group_by(region_name) %>%
  summarise(votesLePen = sum(`LE PEN`),
            votesMacron = sum(MACRON),
            .groups='drop')

round_2_bis %>% group_by(region_name, candidate) %>%
  summarise(votes = sum(votes),
            .groups='drop')

round_2_bis %>%
  group_by(dept_name) %>%
  mutate(rank = min_rank(desc(votes))) %>%
  arrange(dept_name, rank) %>%
  mutate(winner = if_else(rank == 1, TRUE, FALSE)) %>%
  select(dept_name, candidate, votes, rank, winner)

"tidyr::spread() : P33
 ============================================================================= "
round_2_bis %>% spread(candidate, votes) %>%
  select(region_name, `LE PEN`, MACRON, everything())



" ============================================================================
                            Abstract Data Visualization
  ============================================================================ "

"The data : P37
 ============================================================================= "
plot_df <- round_2_bis %>% group_by(region_code, region_name, candidate) %>%
  summarise(votes = sum(votes))

plot <- ggplot(plot_df)


"The aesthetics : P39
 ============================================================================= "
plot <- plot +
  geom_col(aes(x = region_name, y = votes, fill = candidate),
           position = 'dodge')


"The scales : P40-42
 ============================================================================= "
plot <- plot +
  scale_y_continuous(labels = number_format(scale = 1/1000000, suffix = 'M'))


"Colors"
plot + scale_fill_brewer(palette = 'Set1')


plot <- plot +
  scale_fill_manual(values = c('#003171', '#ffea00'))


"Decoration Components : P43
============================================================================== "
plot <- plot + labs(title = "Presidential elections of 2017",
                    subtitle = "Votes per region and candidate",
                    caption = "Data source: https://www.data.gouv.fr/en/posts/les-donnees-des-elections/",
                    y = "Number of votes", x = "Region") +
  guides(fill = guide_legend(title = 'Candidate'))

plot



"Summarized chunk code of the bar chart : P45
============================================================================== "
ggplot(plot_df) +
  geom_col(aes(x = region_name, y = votes, fill = candidate),   # geometric object
           position = 'dodge') +
  scale_y_continuous(labels = number_format(scale = 1/1000000,  # y axis format
                                            suffix = 'M')) +
  scale_fill_manual(values = c('#003171', '#ffea00')) +         # fill colors
  theme_bw() +                                                  # theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom') +
  labs(title = "Presidential elections of 2017",                # title and labels
       subtitle = "Votes per region and candidate",
       caption = "Data source: https://www.data.gouv.fr/en/posts/
       les-donnees-des-elections/",
       y = "Number of votes", x = "Region") +
  guides(fill = guide_legend(title = 'Candidate'))              # legend


"Combining geometric objects : P46
============================================================================== "
missing_votes <- round_2 %>%
  distinct(region_code, dept_code, .keep_all = TRUE) %>% # keep only one observation per department
  group_by(region_code, region_name) %>%
  summarise(blank_ballot = sum(blank_ballot), null_ballot = sum(null_ballot),
            absent_voters = sum(absent_voters)) %>%
  gather(category, value, c(3:5))


"Combining geometric objects : P47
============================================================================== "
ggplot(plot_df, aes(x = region_name)) + # common aesthetics
  geom_col(aes(y = votes, fill = candidate), position = 'dodge') +
  # geom_line object for a second variable
  geom_line(data = missing_votes, # new data
            aes(y = value,
                linetype = category,
                group = category)) + # aesthetics
  scale_y_continuous(labels = number_format(scale = 1/1000000,
                                            suffix = 'M')) +
  scale_fill_manual(values = c('#003171', '#ffea00')) +
  theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust = 1),
                     legend.position = 'right') +
  labs(title = "Presidential elections of 2017",
       y = "Number of votes", x = "Region") +
  guides(fill = guide_legend(title = 'Candidate'),
         linetype = guide_legend(title = '')) +       # title of linetype legend
  scale_linetype_discrete(labels = c("Absent Voters", "Blank Ballot",
                                     "Null Ballot"))  # labels for each linetype

"Decomposition components: facets
============================================================================== "
results2 <- results %>%
  gather(candidate, votes, c(ARTHAUD:POUTOU)) %>%
  group_by(region_code, region_name, candidate, round) %>%
  summarise(votes = sum(votes), .groups='drop')


ggplot(results2, aes(x = region_name)) +
  geom_col(aes(y = votes, fill = candidate),
           position = 'fill') +                       # to generate stacked bars
  scale_y_continuous(labels = percent_format()) +     # y axis format as percent
  scale_fill_brewer(palette = 'Paired') +
  theme_bw() + theme(legend.position = 'bottom') +
  labs(title = "Results of presidential elections of 2017",
       y = "Proportion of votes", x = "Region") +
  guides(fill = guide_legend(title = 'Candidate'),
         linetype = guide_legend(title = '')) +
  scale_linetype_discrete(labels = c("Absent Voters", "Blank Ballot", "Null Ballot")) +
  # define cols as the number of different values for the variable "round"
  facet_grid(cols = vars(round)) +
  coord_flip()                                          # flip coordinate system


" ============================================================================
                            Geospatial Data
  ============================================================================ "


"The package sf
  ============================================================================ "





round_1_bis <- round_1 %>% select(dept_code, dept_name, muni_code, muni_name, everything(), -gender, -firstname) %>%
  rename(commune_code = muni_code, commune_name = muni_name) %>%
  spread(name, votes)
write_csv(round_1_bis, 'data/results_pres_elections_2017_round_1.csv')

round_2_bis <- round_2 %>% select(dept_code, dept_name, muni_code, muni_name, everything(), -gender, -firstname) %>%
  rename(commune_code = muni_code, commune_name = muni_name) %>%
  spread(name, votes)
write_csv(round_2_bis, 'data/results_pres_elections_2017_round_2.csv')
