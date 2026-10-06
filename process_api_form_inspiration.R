########################## Skript na zpracování api dat ########################

library(dplyr)
library(tidyr)
library(purrr)
library(httr)
library(jsonlite)
library(lubridate)
library(stringr)

setwd("C:/Users/JuiceUP/OneDrive - JuiceUP s.r.o/Plocha/Engagement survey/Survey automatizace/API přístup/typeform_API")


token_path <- "token.txt"

##############################################################################
############## Načtení dat

if (!file.exists(token_path)) stop("Token file not found.")
tf_token <- trimws(readLines(token_path, warn = FALSE)[1])
if (nchar(tf_token) == 0) stop("Token file is empty.")


tf_get <- function(page,page_size){
  res <- httr::GET(
    "https://api.typeform.com/forms",
    httr::add_headers(Authorization = paste("Bearer", tf_token)),
    query = list(page = page, page_size = page_size)
  )
  httr::stop_for_status(res)
  
  out <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  items_df <- out$items
}


forms <- tf_get(1,100)


form <- "QG7jds6t"


get_form_definition <- function(tf_token, form_id) {
  url <- paste0("https://api.typeform.com/forms/", form_id)
  res <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", tf_token)))
  httr::stop_for_status(res)
  jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

def <- get_form_definition(tf_token,form)

get_responses_page <- function(tf_token, form_id, page_size = 1000, before = NULL) {
  base <- paste0("https://api.typeform.com/forms/", form_id, "/responses")
  query <- list(page_size = page_size)
  if (!is.null(before) && nzchar(before)) query$before <- before
  
  res <- httr::GET(
    base,
    httr::add_headers(Authorization = paste("Bearer", tf_token)),
    query = query
  )
  httr::stop_for_status(res)
  jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}


responses <- get_responses_page(tf_token,form)



# Fetch ALL responses by walking "before" tokens (descending submitted_at order)
get_all_responses <- function(tf_token, form_id, page_size = 1000, max_pages = 200) {
  all_items <- list()
  before <- NULL
  
  for (i in seq_len(max_pages)) {
    page <- get_responses_page(tf_token, form_id, page_size = page_size, before = before)
    items <- page$items %||% list()
    
    if (length(items) == 0) break
    all_items <- c(all_items, items)
    
    # Typeform includes a per-response "token" used for before/after traversal
    last_token <- items[[length(items)]]$token %||% ""
    if (!nzchar(last_token)) break
    
    # Next page: older than the last token
    before <- last_token
    
    # If we got less than page_size, we're done
    if (length(items) < page_size) break
  }
  
  all_items
}

all_responses <- get_all_responses(tf_token,form)





##################################################################
##### celkový loop - zpracování odpovědí


odpoved_klasif <- function(o){
  if(o[["field"]][["type"]] == 'multiple_choice'){
    return(as.character(o[["choice"]][["label"]]))
  }
  else if(o[["field"]][["type"]] == 'long_text'){
    return(as.character(o[["text"]]))
  }
  else{
    return(as.character(o[["number"]]))
  }
}


###### 
###### tvorba listu + long dataframe

preprocess_data <- function(ls){
  
  processed_list <- list()
  
  ### loop přes respondenty
  for(i in seq_along(ls)){
    
    
    respondent <- ls[[i]]
    
    resp_data <- list(
      respondent_id  = respondent[["response_id"]],
      landed_at    = respondent[["landed_at"]],
      submitted_at = respondent[["submitted_at"]]
    )
    
    respondent_x <- list()
    ### loop přes odpovědi uvnitř respondenta
    for(j in seq_along(respondent[["answers"]])){
      
      odp <- respondent[["answers"]][[j]]
      
      odpoved <- list(
        respondent_id = resp_data$respondent_id,
        landed_at = resp_data$landed_at,
        submitted_at = resp_data$submitted_at,
        typ_otazky = odp[["field"]][["type"]],
        otazka_ref = odp[["field"]][["ref"]],
        odpoved_hodnota = odpoved_klasif(odp)
      )
      
      respondent_x[[j]] <- odpoved
      
    }
    
    processed_list[[i]] <- respondent_x
  }
  
  return(processed_list)
}



survey_data_preprocessed <- preprocess_data(all_responses)


survey_df_long <- dplyr::bind_rows(unlist(survey_data_preprocessed, recursive = FALSE))



#######
###### číselník skupin


add_group_title <- function(f){
  if(f[["type"]] == 'group'){
    return(as.character(f[["title"]]))
  }
  else{
    return(NA_character_)
  }
}

add_group_ref <- function(f){
  if(f[["type"]] == 'group'){
    return(as.character(f[["ref"]]))
  }
}


add_q_title <- function(f){
  
  if(f[["type"]] == 'group'){
    return(as.character(f[["ref"]]))
  }
  
}


process_survey_ciselnik <- function(ls){
  
  k <- 1
  
  survey_list <- list()
  
  for(i in seq_along(ls[["fields"]])){
    
    field <- ls[["fields"]][[i]]
    
    if(field[["type"]] == 'group'){
      
      group_data <- list(
        group_title = add_group_title(field),
        group_ref = add_group_ref(field)
      )
      
      
      for(j in seq_along(field[["properties"]][["fields"]])){
        
        ot <- field[["properties"]][["fields"]][[j]]
        
        survey_list[[k]] <- list(
          
          group_title = group_data$group_title,
          group_ref = group_data$group_ref,
          otazka_title = ot[["title"]],
          otazka_ref = ot[["ref"]]
        )
        k <- k+1
      }
    }
    
    else if(field[["type"]] == 'statement') next
    
    else{
      
      survey_list[[k]] <- list(
        group_title = NA_character_,
        group_ref = NA_character_,
        otazka_title = field[["title"]],
        otazka_ref = field[["ref"]]
      )
      
      k <- k+1
    }
    
  }
  return(survey_list)
  
}

ciselnik_list <- process_survey_ciselnik(def)

ciselnik_df <- dplyr::bind_rows(ciselnik_list)



#=============================================================
# Clustering otázek
#=============================================================

standardize_columns <- function(df){
  cols <- df %>% pull(otazka_title)
  
  cols_stand <- cols %>%
    tolower() %>%
    stringi::stri_trans_general("Latin-ASCII")%>%
    stringr::str_replace_all("\\.","_")%>%
    stringr::str_replace_all(" ","_")%>%
    stringr::str_replace_all("\\?","")%>%
    stringr::str_replace_all("_+","_")
  
  return(cols_stand)
}

q_unique <- standardize_columns(ciselnik_df)


#----- clustering


q_mat <- stringdist::stringdistmatrix(q_unique,q_unique,method="cosine",
                                      q = 5)
colnames(q_mat) <- q_unique
rownames(q_mat) <- q_unique

q_labels_short <- stringr::str_trunc(q_unique, 15)

fig <- plot_ly(
  x = q_labels_short,
  y = q_labels_short,
  z = q_mat,
  type = "heatmap"
)
fig


q_dist <- as.dist(q_mat)

hc <- hclust(q_dist,method="average")
plot(hc,labels=q_labels_short)



#--------- párování otázek

question_dist_df <- as.data.frame(q_mat)






### funkce na standardizování textu

clean_text <- function(x) {
  stringr::str_squish(stringr::str_trim(x))
}



final_long_df <- survey_df_long %>%
  left_join(ciselnik_df, by = "otazka_ref")%>%
  mutate(otazka_title = clean_text(otazka_title))



######### další zpracování - specifické pro engagement survey

multiple_choice_extra <- final_long_df %>% 
  filter(typ_otazky == "multiple_choice") %>%
  summarise(n = n_distinct(otazka_ref))


open <- final_long_df %>%
  filter(typ_otazky == "rating") %>%
  summarise(n = n_distinct(otazka_ref))

### identifikace sekcí helpers

  
include_multiple_choice <- function(df){
  
  multiple_choice_extra <- df %>% 
    filter(typ_otazky == "multiple_choice") %>%
    summarise(n = n_distinct(otazka_ref))
  
  if(multiple_choice_extra$n > 3){
    return(TRUE)
  }
  else{return(FALSE)}
  
}

mc_present <- include_multiple_choice(final_long_df)


include_openended <- function(df){
  openend <- df %>%
    filter(typ_otazky == "long_text") %>%
    summarise(n = n_distinct(otazka_ref))
  
  if(openend$n > 0){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


openend_present <- include_openended(final_long_df)

###################################
######### zpracování odpovědí


##### demografie

zakl_info <- final_long_df %>%
  filter(group_title == "Základní info" & typ_otazky == "multiple_choice")%>%
  dplyr::select(odpoved_hodnota,otazka_title,respondent_id)

zakl_info_otazky <- unique(zakl_info$otazka_title)

## průměrná doba vyplnění

doba_vyplneni <- final_long_df %>%
  dplyr::select(respondent_id,landed_at,submitted_at)%>%
  group_by(respondent_id) %>%
  mutate(
    landed_at = ymd_hms(landed_at),
    submitted_at = ymd_hms(submitted_at),
    cas_vyplneni = submitted_at - landed_at
  )%>%
  distinct(respondent_id,cas_vyplneni)

avg_doba_vyplneni <- mean(doba_vyplneni$cas_vyplneni)
avg_doba_vyplneni <- round(as.numeric(avg_doba_vyplneni, units = "mins"),0)


#### počet respondentů

pocet_resp <- length(unique(final_long_df$respondent_id))


#### návratnost 

## respondenti per oddělení



oddeleni_expr <- "jakém"

oddeleni_index <- str_detect(zakl_info_otazky,oddeleni_expr)

oddeleni_ot <- zakl_info_otazky[oddeleni_index]

oddeleni <- zakl_info %>%
  filter(otazka_title == oddeleni_ot) %>%
  group_by(respondent_id) %>%
  summarise(odpoved_hodnota = first(na.omit(odpoved_hodnota))) %>%
  count(odpoved_hodnota, name = "pocet_respondentu") %>%
  mutate(pocet_zamestnancu = 0)


odd_souhrn_long <- oddeleni %>%
  pivot_longer(cols= 2:3, names_to = "Kategorie", values_to = "Pocet")


odd_souhrn_long$oddeleni_trunc <- str_trunc(odd_souhrn_long$odpoved_hodnota, 
                                            width = 25, ellipsis = "...")





#### věk


vek_souhrn <- final_long_df %>%
  filter(group_title == "Základní info" & typ_otazky == "number") %>%
  dplyr::select(respondent_id,odpoved_hodnota) %>%
  mutate(odpoved_hodnota = as.numeric(odpoved_hodnota))%>%
  group_by(respondent_id)%>%
  summarise(odpoved_hodnota = first(na.omit(odpoved_hodnota)))%>%
  count(odpoved_hodnota,name = "pocet")%>%
  rename(vek = odpoved_hodnota)
  

#### pozice

pozice_expr <- "jaké\\s"

pozice_index <- str_detect(zakl_info_otazky, pozice_expr)

pozice_ot <- zakl_info_otazky[pozice_index]

pozice_souhrn <- zakl_info %>%
  filter(otazka_title == pozice_ot)%>%
  group_by(respondent_id)%>%
  summarise(odpoved_hodnota = first(na.omit(odpoved_hodnota)))%>%
  count(odpoved_hodnota,name="pocet")%>%
  rename(pozice = odpoved_hodnota)



#### seniorita

doba_expr <- "dlouho"

az_expr <- "\\d+(?=\\-{1})" ### popř. az_expr <- "\\d+(?= až{1})"

do_expr <- "do\\s\\d{1}"

vice_expr <- "více"

doba_index <- str_detect(zakl_info_otazky, doba_expr)

doba_ot <- zakl_info_otazky[doba_index]


  
doba_souhrn <- zakl_info %>%
  filter(otazka_title == doba_ot)%>%
  group_by(respondent_id)%>%
  summarise(odpoved_hodnota = first(na.omit(odpoved_hodnota)))%>%
  count(odpoved_hodnota,name="pocet")%>%
  mutate(az = case_when(
    str_detect(odpoved_hodnota,do_expr) == T ~ 0,
    str_detect(odpoved_hodnota,vice_expr) == T ~ 100,
    .default = as.integer(str_extract(odpoved_hodnota,az_expr))
  )) %>%
  arrange(az) %>%
  mutate(odpoved_hodnota = factor(odpoved_hodnota, levels = odpoved_hodnota))%>%
  rename(doba=odpoved_hodnota)



  
  
##### engagement index celkem

eng_otazky <- ciselnik_df %>%
  filter(group_title == "Engagement") %>%
  pull(otazka_title)

engagement_index_celkem <- final_long_df %>%
  filter(group_title == "Engagement") %>%
  mutate(odpoved_hodnota = as.numeric(odpoved_hodnota),
         uroven_engagementu = as.factor(case_when(
           odpoved_hodnota < 2 ~ "Úplný engagement",
           odpoved_hodnota >= 2 & odpoved_hodnota <= 3 ~ "Částečný engagement",
           odpoved_hodnota > 3 ~ "Slabý engagement"
         ))
  ) %>%
  group_by(uroven_engagementu) %>%
  summarise(n = n()) %>%
  mutate(pct = round((n/sum(n))*100,1))

engagement_index_otazky$otazka_title <- factor(engagement_index_otazky$otazka_title, levels = eng_otazky)

uplny_engagement <- engagement_index_celkem %>%
  filter(uroven_engagementu == "Úplný engagement")%>%
  pull(pct)

castecny_engagement <- engagement_index_celkem %>%
  filter(uroven_engagementu == "Částečný engagement")%>%
  pull(pct)

slaby_engagement <- engagement_index_celkem %>%
  filter(uroven_engagementu == "Slabý engagement")%>%
  pull(pct)

#### engagementové otázky
eng_otazky <- ciselnik_df %>%
  filter(group_title == "Engagement") %>%
  pull(otazka_title)


engagement_index_otazky <- final_long_df %>%
  filter(group_title == "Engagement") %>%
  mutate(odpoved_hodnota = as.numeric(odpoved_hodnota),
         uroven_engagementu = as.factor(case_when(
           odpoved_hodnota < 2 ~ "Úplný engagement",
           odpoved_hodnota >= 2 & odpoved_hodnota <= 3 ~ "Částečný engagement",
           odpoved_hodnota > 3 ~ "Slabý engagement"
         ))) %>%
  group_by(uroven_engagementu,otazka_title) %>%
  summarise(n = n()) %>%
  group_by(otazka_title) %>%
  mutate(pct = round((n/sum(n))*100,1),
         otazka_title = factor(otazka_title, levels = rev(eng_otazky))) %>%
  ungroup()

### úroveň odpovědí
eng_order <- c("Úplný engagement","Částečný engagement","Slabý engagement")

engagement_index_otazky$uroven_engagementu <- factor(engagement_index_otazky$uroven_engagementu,
                                                     levels = rev(eng_order))


##### engagement by oddělení






##### individuální míra engagementu
ind_engagement <- final_long_df %>%
  filter(group_title == "Engagement") %>%
  group_by(respondent_id) %>%
  mutate(
    odpoved_hodnota = as.numeric(odpoved_hodnota),
    prum_eng = mean(odpoved_hodnota),
    ind_mira = case_when(
      prum_eng <= 1.2 ~ "Engaged",
      prum_eng > 1.2 & prum_eng <= 2.5 ~ "Potential",
      prum_eng > 2.5 ~ "Disengaged"
    )) %>%
  ungroup() %>%
  distinct(respondent_id,ind_mira) %>%
  group_by(ind_mira) %>%
  summarise(n = n()) %>%
  mutate(pct = round((n/sum(n))*100,1))

engaged <- ind_engagement%>%filter(ind_mira == "Engaged")%>%pull(pct)
potential <- ind_engagement%>%filter(ind_mira == "Potential")%>%pull(pct)
disengaged <- ind_engagement%>%filter(ind_mira == "Disengaged")%>%pull(pct)

############# drivery 

drivery_overview <- final_long_df %>%
  filter(group_title != "Základní info" & group_title != "Engagement"
         & typ_otazky == "opinion_scale") %>%
  mutate(odpoved_kat = case_when(
    odpoved_hodnota <= 2 ~ "Skvělý výsledek",
    odpoved_hodnota == 3 ~ "Průměrný výsledek",
    odpoved_hodnota > 3 ~ "Špatný výsledek"
  ))%>%
  group_by(group_title, odpoved_kat) %>%
  summarise(n = n()) %>%
  group_by(group_title) %>%
  mutate(pct = round((n/sum(n))*100,1))


drivery_sort <- drivery_overview %>%
  filter(odpoved_kat == "Skvělý výsledek") %>%
  arrange(desc(pct)) %>%
  ungroup() %>%
  mutate(poradi = row_number())

drivery_overview_complete <- drivery_overview %>%
  left_join(drivery_sort %>% select(group_title,poradi), by = "group_title") %>%
  arrange(poradi)


drivery_otazky <- final_long_df %>%
  filter(group_title != "Základní info" & group_title != "Engagement"
         & typ_otazky == "opinion_scale") %>%
  mutate(odpoved_kat = case_when(
    odpoved_hodnota <= 2 ~ "Skvělý výsledek",
    odpoved_hodnota == 3 ~ "Průměrný výsledek",
    odpoved_hodnota > 3 ~ "Špatný výsledek"
  ))%>%
  group_by(group_title,otazka_title,odpoved_kat)%>%
  summarise(n = n())%>%
  group_by(otazka_title)%>%
  mutate(pct = round((n/sum(n))*100,2))%>%
  left_join(drivery_sort%>%select(group_title,poradi), by = "group_title")





############### list s celkovými výsledky driveru i detailně pro otázky

get_driver_results <- function(data,place){
  data %>%
    filter(poradi == place)
}

get_question_results <- function(data,place){
  data %>%
    filter(poradi==place)
}


driver_split <- split(drivery_overview_complete, drivery_overview_complete$poradi)
question_split <- split(drivery_otazky, drivery_otazky$poradi)

driver_list <- Map(function(driver,question){
  list(
    driver_result = driver,
    question_result = question
  )
},driver_split,question_split)

d1 <- driver_list[["1"]][["question_result"]]



####### načtení benchmarků

bench_form <- "m4MXWOXz"

bench_def <- get_form_definition(tf_token,bench_form)

bench_responses <- get_responses_page(tf_token,bench_form)

bench_all_responses <- get_all_responses(tf_token,bench_form)

bench_data_preprocessed <- preprocess_data(bench_all_responses)

bench_df_long <- dplyr::bind_rows(unlist(bench_data_preprocessed, recursive = FALSE))

bench_ciselnik_list <- process_survey_ciselnik(bench_def)

bench_ciselnik_df <- dplyr::bind_rows(bench_ciselnik_list)


bench_df_complete <- bench_df_long %>%
  left_join(bench_ciselnik_df, by = "otazka_ref")%>%
  mutate(odpoved_hodnota = as.numeric(odpoved_hodnota),
         otazka_title = clean_text(otazka_title))

###### jednotlivé benchmarky engagementu

bench_cr_val <- bench_df_complete %>% filter(otazka_title=="Česká republika")%>%
  pull(odpoved_hodnota)

bench_cr_name <- bench_df_complete %>% filter(otazka_title=="Česká republika")%>%
  pull(otazka_title)


bench_us_val <- bench_df_complete %>% filter(otazka_title=="USA")%>%
  pull(odpoved_hodnota)

bench_us_name <- bench_df_complete %>% filter(otazka_title=="USA")%>%
  pull(otazka_title)

bench_ger_val <- bench_df_complete %>% filter(otazka_title=="Německo")%>%
  pull(odpoved_hodnota)

bench_ger_name <- bench_df_complete %>% filter(otazka_title=="Německo")%>%
  pull(otazka_title)

### aktuální výsledky vs benchmarky

#drivery_clean <- drivery_otazky %>%
#  mutate(otazka_title = str_squish(str_trim(otazka_title)))

#bench_clean <- bench_df_complete %>%
#  mutate(otazka_title = str_squish(str_trim(otazka_title)))

drivery_benchmark <- drivery_otazky %>%
  dplyr::select(otazka_title,odpoved_kat,pct)%>%
  left_join(
    bench_df_complete %>% select(otazka_title,odpoved_hodnota),
    by = "otazka_title"
  ) %>%
  filter(odpoved_kat == "Skvělý výsledek")%>%
  mutate(pod_benchmarkem = case_when(
    odpoved_hodnota == NA ~ NA,
    pct < odpoved_hodnota ~ TRUE,
    pct >= odpoved_hodnota ~ FALSE
  ))


include_horsi <- function(df){
  if(any(df$pod_benchmarkem, na.rm = T)){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

horsi_vysledky_present <- include_horsi(drivery_benchmark)


agenda_vector <- c(horsi_vysledky_present,mc_present,openend_present)


agenda <- function(x){
  if(identical(x, c(T, T, T))){
    print(1)
  }
  else if(identical(x, c(F,T,T))){
    print(2)
  }
  else if(identical(x, c(T,F,T))){
    print(3)
  }
  else if(identical(x, c(T,T,F))){
    print(4)
  }
  else if(identical(x, c(F,F,T))){
    print(5)
  }
  else if(identical(x, c(F,T,F))){
    print(6)
  }
  else if(identical(x, c(T,F,F))){
    print(7)
  }
  else{
    print(8)
  }
  
}

agenda(agenda_vector)


identify_pocet_sekci <- function(x){
  if(identical(x, c(T, T, T))){
    return(7)
  }
  else if(identical(x, c(F,T,T))){
    return(6)
  }
  else if(identical(x, c(T,F,T))){
    return(6)
  }
  else if(identical(x, c(T,T,F))){
    return(6)
  }
  else if(identical(x, c(F,F,T))){
    return(5)
  }
  else if(identical(x, c(F,T,F))){
    return(5)
  }
  else if(identical(x, c(T,F,F))){
    return(5)
  }
  else{
    return(4)
  }
  
}

pocet_sekci <- identify_pocet_sekci(agenda_vector)


build_agenda <- function(x) {
  
  base_sections <- c(
    "Průzkum v číslech",
    "Výsledky engagementu",
    "Výsledky jednotlivých driverů"
  )
  
  optional_sections <- c()
  
  if (x[1]) {
    optional_sections <- c(optional_sections, "Otázky, které dopadly hůře")
  }
  
  if (x[2]) {
    optional_sections <- c(optional_sections, "Multiple choice otázky")
  }
  
  if (x[3]) {
    optional_sections <- c(optional_sections, "Otevřené otázky")
  }
  
  final_section <- "Závěrečná shrnutí a doporučení"
  
  sections <- c(base_sections, optional_sections, final_section)
  
  list(
    sections = sections,
    total_sections = length(sections),
    progress_total = length(sections) + 1
  )
}

agenda_cfg <- build_agenda(c(F,F,T))

section_positions <- setNames(seq_along(agenda_cfg$sections), agenda_cfg$sections)


get_progress <- function(agenda_cfg, section_name) {
  pos <- match(section_name, agenda_cfg$sections)
  list(
    filled = pos,
    total = agenda_cfg$progress_total
  )
}

mc_progress <- get_progress(agenda_cfg, "Výsledky engagementu")

mc_progress$filled
