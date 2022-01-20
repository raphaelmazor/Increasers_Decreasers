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

