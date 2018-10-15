# Join existing data on border barriers to our source data
# 
# Data from: 
# barriers.bw - Jones, Reece (2012) Border Walls: Security and the War on Terror, London: Zed Books, p. 10.
# barriers.wiki - https://en.wikipedia.org/wiki/Border_barrier
# barriers.hw - https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/U16NEI

# Source data created in "SourceData.R"

# Select variables to keep for the join
vars <- c("state1", "state2", "year", "indicator", "source")

barriers.bw.join <- barriers.bw %>%
  select(vars) %>%
  mutate(year = as.character(year)) %>%
  rename(built = year)

barriers.wiki.join <- barriers.wiki %>%
  select(vars) %>%
  rename(built = year)

# Join
# barrier.bw
border.df <- border.df %>%
  left_join(barriers.bw.join, by = c("state1", "state2"))

# barriers.wiki
border.df <- border.df %>%
  left_join(barriers.wiki.join, by = c("state1", "state2"), suffix = c(".bw", ".wiki"))