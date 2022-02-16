library(CSCI)
library(tidyverse)

#generate example output
example(CSCI)
meta_df<-loadMetaData()

results$Suppl1_OE %>%
  mutate(Type="Type")%>%
  # as_tibble() %>%
  # mutate(Rand1=runif(nrow(.)),
  #   SimObserved1=case_when(Rand1<CaptureProb~1,T~0))
  group_by(Type, OTU) %>%
  summarise(Si = sum(CaptureProb),
            N = SampleID %>% unique %>% length(),
            Fe = Si/N,
            Fo = sum(MeanObserved>0)/N,
            Df=Fo-Fe) %>%
  ungroup() %>%
  mutate(Df=Fo-Fe,
         Fo_Fe = Fo/Fe,
         Type = case_when(Df>0~"Increaser",
                          Df<0~"Decreaser",
                          Df==0~"Neutral",
                          T~"Error")) %>%

  arrange(Df)

# Traits


# Nathan's addition 02162022-------------------------------------------------------
report <- CSCI(bugs, stations)

md <- loadMetaData() %>% 
  select(FinalID, SAFIT1__OTU_a, FunctionalFeedingGroup) %>% 
  filter(!FunctionalFeedingGroup == '')

data <- report$Suppl1_OE %>% 
  mutate(year = year(mdy(str_sub(.$SampleID, start = 11)))) %>% 
  mutate(location = str_sub(.$StationCode, start = -1)) %>% #location codes a A=above, B=below, R=river, from SWAMP code
  mutate(stream = str_sub(.$StationCode, start = 7, end = 7)) %>%  
#Rafi's code below
  group_by(location,OTU) %>% #any kind of grouping can be added here
  summarise(Si = sum(CaptureProb),
            N = SampleID %>% unique %>% length(),
            Fe = Si/N,
            Fo = sum(MeanObserved>0)/N,
            Df=Fo-Fe) %>%
  ungroup() %>% 
  mutate(Df=Fo-Fe,
         Type= case_when(Df> 0 ~'increasers', #possibly change these thresholds
                         Df< 0 ~'decreasers',
                         Df == 0 ~ 'neutral',
                         T~ 'error')) %>% 
  mutate(FFG = md$FunctionalFeedingGroup[match(.$OTU, md$SAFIT1__OTU_a)]) %>% 
  arrange (Df)
#give a pivot table of which ffg are increasing and decreasing by sample location
#change group_by() for different variable
data_ffg <- data %>% group_by(location, Type, FFG) %>% summarise(n = n()) %>% 
  pivot_wider(names_from = Type, values_from = n) %>% 
  select(location, FFG, increasers, decreasers, everything())
