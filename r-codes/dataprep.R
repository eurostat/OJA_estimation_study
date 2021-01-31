rm(list = ls())

#########################################
# LOAD LIBRARIES

library(RAthena)
library(tidyverse)
library(restatapi)

#########################################
# LOAD OTHER FUNCTIONS


source("r-codes/functions.R")

#########################################
# GET JVS DATA FROM EUROSTAT

country_names<-get_eurostat_dsd("jvs_q_isco_r2")[,c(2:3)]
country_names$name<-gsub("Germany.*","Germany",country_names$name)
eu27<-get("cc",.restatapi_env)$EU27_2020


jvs <- get_eurostat_data("jvs_q_isco_r2",filters=list(s_adj="NSA",indic_em="JOBVAC"),date_filter = "2018<") %>% # 
  select(-c(s_adj, indic_em)) %>%
  spread(sizeclas, values) %>%
  replace(is.na(.), 0) %>%
  mutate(EU_sizeclas_max = if_else(TOTAL > GE10, TOTAL, GE10),
         isco08 = as.character(sub("OC", "", isco08)),
         geo = as.character(geo),
         nace_r2 = as.character(nace_r2)) %>%
  select(-c(GE10, TOTAL)) %>%
  spread(isco08, EU_sizeclas_max) %>%
  replace(is.na(.), 0) %>%
  mutate(`6` = `6_7` - `7`) %>%
  gather(isco08, EU_sizeclas_max, 4:ncol(.)) %>%
  left_join(country_names, c("geo" = "code")) %>%
  select(name, geo, time, nace_r2, isco08, EU_sizeclas_max) %>%
  rename(countries = "name") %>%
  arrange(countries, nace_r2, isco08, time)  %>%
  filter(geo %in% eu27)

#########################################
# GET OJA DATA FROM CEDEFOP

# `aws` contains the authentication parameters to the Athena service
source(".aws.R")
con <- dbConnect(RAthena::athena(),
                region_name=aws$RegionName,
                aws_access_key_id = aws$AccessKeyId,
                aws_secret_access_key = aws$SecretAccessKey,
                aws_session_token = aws$Token,
                s3_staging_dir=aws$S3StagingDir,
                work_group = aws$WorkGroup
)


query<-"SELECT 
 general_id, idcountry, grab_date, expire_date, idesco_level_2, nace, 
 /*idworking_hours, working_hours, idcontract, contract, ideducational_level, educational_level,*/
 source, source_category, 
 count(*) as N
 FROM (
 SELECT *, idmacro_sector AS nace
   FROM estat_dsl2531b_oja.ft_document_en 
   WHERE idmacro_sector NOT IN ('', 'T', 'U') AND 
   idesco_level_2 NOT in ('') AND 
   (grab_date <= expire_date) AND
   idcountry NOT IN ('UK') AND
   expire_date < 18352 AND  expire_date > 17804 
 )
 GROUP BY general_id, idcountry, grab_date, expire_date, idesco_level_2, nace, 
 /*idworking_hours, working_hours, idcontract, contract, ideducational_level, educational_level,*/
 source, source_category
"
oja_db<-get_data(query)
oja_db[, ":="(grab_date = as.Date(grab_date, origin = "1970-01-01"),
              expire_date = as.Date(expire_date, origin = "1970-01-01"))]
oja_db[, ":="(t2018Q3 = expire_date >= as.Date("2018-09-30") & grab_date <= as.Date("2018-09-30"),
               t2018Q4 = expire_date >= as.Date("2018-12-31") & grab_date <= as.Date("2018-12-31"),
               t2019Q1 = expire_date >= as.Date("2019-03-31") & grab_date <= as.Date("2019-03-31"),
               t2019Q2 = expire_date >= as.Date("2019-06-30") & grab_date <= as.Date("2019-06-30"),
               t2019Q3 = expire_date >= as.Date("2019-09-30") & grab_date <= as.Date("2019-09-30"),
               t2019Q4 = expire_date >= as.Date("2019-12-31") & grab_date <= as.Date("2019-12-31"))]

oja_db[, ":="(t2018Q3_days = ifelse(t2018Q3, as.Date("2018-09-30")-grab_date, NA),
               t2018Q4_days = ifelse(t2018Q4, as.Date("2018-12-31")-grab_date, NA),
               t2019Q1_days = ifelse(t2019Q1, as.Date("2019-03-31")-grab_date, NA),
               t2019Q2_days = ifelse(t2019Q2, as.Date("2019-06-30")-grab_date, NA),
               t2019Q3_days = ifelse(t2019Q3, as.Date("2019-09-30")-grab_date, NA),
               t2019Q4_days = ifelse(t2019Q4, as.Date("2019-12-31")-grab_date, NA))]

oja_db[, count_quarts:=t2018Q3+t2018Q4+t2019Q1+t2019Q2+t2019Q3+t2019Q4]
oja_db[, .N , keyby=count_quarts]

ss<-function(x){
  oja_db[(eval(parse(text=x))),.(idcountry=idcountry,idesco_level_2=as.numeric(idesco_level_2),nace=nace,quarters=sub("t","",x),N=N)]
}
quarts<-c("t2018Q3","t2018Q4","t2019Q1","t2019Q2","t2019Q3","t2019Q4")
oja_dbq<-rbindlist(lapply(quarts,ss))




oja<-oja_dbq[!is.na(quarters),.(vals=.N),by=.(idcountry, idesco_level_2, nace, quarters)]
cols<-c('idesco_level_2','vals')
oja[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]


oja <- oja %>%
  mutate(quarters = recode(quarters, "2018Q3"="2018-Q3", "2018Q4"="2018-Q4", "2019Q1"="2019-Q1",
                           "2019Q2"="2019-Q2", "2019Q3"="2019-Q3", "2019Q4"="2019-Q4"),
         idesco_level_2 = substr(idesco_level_2, 1, 1),
         idcountry = as.character(idcountry),
         nace = as.character(nace)) %>%
  left_join(country_names, c("idcountry" = "code")) %>%
  rename(countries = "name", geo = "idcountry", time = "quarters", isco08 = "idesco_level_2", nace_r2 = "nace") %>%
  select(countries, geo, time, nace_r2, isco08, vals) %>%
  arrange(countries, nace_r2, isco08, time)


###############################################################
# COUNTRIES OVER TIME

oja_a <- oja %>%  
  group_by(countries, time) %>%  
  summarise(sum = sum(vals)) %>%
  ungroup() %>%
  mutate(countries = as.character(countries),
         # time = as.Date(time),
         sum = as.numeric(sum))

jvs_a<- jvs %>%
  filter(isco08 %in% "TOTAL" & nace_r2 %in% "A-S") %>%
  filter(time %in% c("2018-Q3","2018-Q4","2019-Q1",
                     "2019-Q2","2019-Q3","2019-Q4")) %>%
  select(-c(geo, nace_r2, isco08)) %>% 
  group_by(countries, time) %>%      
  summarise(EU_sizeclas_max = sum(EU_sizeclas_max)) %>%
  ungroup() %>%
  mutate(countries = as.character(countries),
         # time = as.Date(time),
         EU_sizeclas_max = as.numeric(EU_sizeclas_max))


MERGED_a <- merge(x = oja_a,
                  y = jvs_a,
                  by = c("countries", "time"),
                  all.x = T) %>%
  rename(OJA = "sum", JVS = "EU_sizeclas_max") %>%
  replace(is.na(.), 0) %>%
  mutate(time=recode(time, "2018-Q3"="2018-07-01", "2018-Q4"="2018-10-01", "2019-Q1"="2019-01-01",
                     "2019-Q2"="2019-04-01", "2019-Q3"="2019-07-01", "2019-Q4"="2019-10-01")) %>%
  mutate(time=as.Date(time)) %>%
  arrange(countries, time)


###############################################################
# NACE SECTIONS OVER TIME

oja_b <- oja %>%
  select(-c(geo, isco08)) %>%
  mutate(countries = as.character(countries),
         vals = as.numeric(vals)) %>%
  group_by(countries, time, nace_r2) %>%  
  summarise(sum = sum(vals)) %>%
  arrange(countries, nace_r2, time) %>%
  select(nace_r2, time, countries, sum) %>%
  spread(countries, sum) %>%
  replace(is.na(.), 0) %>%
  gather(countries, sum, 3:ncol(.)) 


jvs_b <- jvs %>%
  filter(isco08 %in% "TOTAL" & !nace_r2 %like% "-|_") %>%
  select(-isco08) %>% 
  group_by(countries, time, nace_r2) %>%      
  summarise(EU_sizeclas_max = sum(EU_sizeclas_max)) %>%
  arrange(nace_r2, countries, time)


MERGED_b <- merge(x = oja_b,
                  y = jvs_b,
                  by = c("countries", "time", "nace_r2"),
                  all.x = T) %>%
  rename(OJA = "sum", JVS = "EU_sizeclas_max") %>%
  mutate(time=recode(time, "2018-Q3"="2018-07-01", "2018-Q4"="2018-10-01", "2019-Q1"="2019-01-01",
                     "2019-Q2"="2019-04-01", "2019-Q3"="2019-07-01", "2019-Q4"="2019-10-01")) %>%
  mutate(time=as.Date(time)) %>%
  arrange(nace_r2, time)



###############################################################
# ISCO OCCUPATIONS OVER TIME       

oja_c <- oja %>%  
  mutate(countries = as.character(countries),
         vals = as.numeric(vals)) %>%
  select(-c(nace_r2, geo)) %>%
  group_by(countries, time, isco08) %>%  
  summarise(sum = sum(vals, na.rm = T)) %>%
  spread(countries, sum) %>%
  replace(is.na(.), 0) %>%
  gather(countries, sum, 3:ncol(.)) 



jvs_c <- jvs %>%
  filter(!nace_r2 %like% "-|_" & !isco08 %like% "-|_|TOTAL|UNK|0") %>%
  select(-c(nace_r2, geo)) %>%
  mutate(time = as.character(time)) %>%
  group_by(countries, time, isco08) %>%      
  summarise(EU_sizeclas_max = sum(EU_sizeclas_max)) 



MERGED_c <- merge(x = oja_c,
                   y = jvs_c,
                   by = c("countries", "time", "isco08"),
                   all.x = T) %>%
  mutate(time=recode(time, "2018-Q3"="2018-07-01", "2018-Q4"="2018-10-01", "2019-Q1"="2019-01-01",
                     "2019-Q2"="2019-04-01", "2019-Q3"="2019-07-01", "2019-Q4"="2019-10-01")) %>%
  mutate(time=as.Date(time)) %>%
  rename(OJA = "sum", JVS = "EU_sizeclas_max")


###############################################################
# Smaller dataset for calculations


oja_capture <- oja_db[!idcountry %in% c("CY", "DK", "EE", "LV", "EL", "MT", "SI", "HR") & count_quarts > 0]
jvs <- restatapi::get_eurostat_data("jvs_q_isco_r2",filters=list(indic_em="JOBVAC",geo=eu27,isco08="TOTAL",sizeclas="TOTAL",nace_r2="[A-Z]"),date_filter = "2018<") %>%
  count(geo, s_adj, time, wt = values, name = "jvs") %>%
  mutate(yq = gsub("-","",time))



