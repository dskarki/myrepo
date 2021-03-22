df_shopville=Shopville
summary(df_shopville)


library(dplyr)

new_df <- mutate(df_shopville, Strata =case_when(Location == "MM" ~ 'S1',
                                       Location=="SM" ~ 'S2',
                                       TRUE ~ 'S3'))

new_df_1 <- mutate(new_df, fpc =case_when(Location == "MM" ~ '27',
                                       Location=="SM" ~ '28',
                                       TRUE ~ '35'))
final_df= mutate(new_df_1, weight =case_when(Location == "MM" ~ '3.375',
                                        Location=="SM" ~ '7',
                                        TRUE ~ '35/6'))
library(survey)
final_df$Strata=as.factor(final_df$Strata)
final_df$fpc=as.integer(final_df$fpc)
#final_df$weight <- as.numeric(as.character(final_df$weight))(no need to do it)
my_design_df=svydesign(id=~1, strata=~Strata,data=final_df,fpc=~fpc)
#calculating point estimate for total revenue
totalrev=svytotal(~FebRev,design=my_design_df,deff=TRUE)
totalrev
CI_totalrev=confint(totalrev,level=0.90, df=degf(my_design_df))
CI_totalrev
#calculating point estimate for average square footage
avgfoot=svymean(~SqFt,design=my_design_df,deff=TRUE)
avgfoot
CI_avgfoot=confint(avgfoot, level=0.90, df=degf(my_design_df))
CI_avgfoot

