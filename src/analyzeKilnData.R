source('src/getKilnData2019.R')

rm(list=setdiff(ls(), c("df_merged",
                        "df_defects",
                        "df_yields",
                        
                        "kilns_AB",
                        "kilns_C",
                        "kilns_D",
                        "kilns_E",
                        "kilns_F",
                        "kilns_G",
                        "kilns_H"
                        )))

names(kilns_AB)
names(kilns_C)
names(kilns_D)
names(kilns_E)
names(kilns_F)
names(kilns_G)
names(kilns_H)