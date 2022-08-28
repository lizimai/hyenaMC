fetch_dyad_is.rule.fitting.mate <- function(femaleID, maleID) {
  
  input <- tibble::tibble(femaleID, maleID)
  
  input %>%
    dplyr::arrange() %>%
    dplyr::distinct() %>%
    dplyr:: mutate(female_birthdate = hyenaR::fetch_id_date.birth(.data$femaleID),
                   female_birthclan = hyenaR::fetch_id_clan.birth(.data$femaleID),
                   male_birthdate = hyenaR::fetch_id_date.birth(.data$maleID),
                   male_clan_on_female_birthdate = hyenaR::fetch_id_clan.current(.data$maleID, .data$female_birthdate),
                   # does father fit the rule depnends on whether he was not present in
                   # the same clan when mother was born or he was not born on mother
                   # birthdate TODO: set a margin for stricter rule criteria: before how
                   # many days after mother birthdate the father should not be present
                   is_fitting = .data$male_clan_on_female_birthdate != .data$female_birthclan | .data$male_birthdate > .data$female_birthdate,
                   is_fitting = ifelse(.data$female_birthdate < "1996-04-12" & .data$male_birthdate < "1996-04-12", NA_real_, .data$is_fitting)) %>% # type of founder males cannot be determined
    dplyr::left_join(x = input, y = ., by = c("femaleID", "maleID")) -> output
  
  output$is_fitting
}

fetch_id_parity <- function(femaleID, date) {
  
  input <- tibble::tibble(femaleID = femaleID,
                 date = date)
  
  femaleIDUnique <- unique(sort(femaleID))
  
  create_offspring_litterID.from.all(parentID = femaleIDUnique) %>% 
    filter(!fetch_id_is.censored(parentID) & (filiation == "mother_social_genetic" | filiation == "mother_genetic" | filiation == "father")) %>% 
    select(parentID, birthdate) %>% 
    arrange(parentID, birthdate) %>%
    unique() %>% 
    group_by(parentID) %>% 
    mutate(conceptionDate = birthdate - 110,
           nextConceptionDate = dplyr::lead(conceptionDate, 1) -1,
           litterSeq = row_number()) %>% 
    ungroup() %>% 
    select(parentID, conceptionDate, nextConceptionDate, litterSeq)-> litterTbl

  input %>% 
    arrange() %>% 
    unique() %>% 
    left_join(.,litterTbl,  by = c("femaleID"= "parentID"))  %>% 
    filter((date >= conceptionDate & date < nextConceptionDate) | (date >= conceptionDate & is.na(nextConceptionDate))) -> parityTbl
  
  
  left_join(input, parityTbl, by = c("date" = "date", "femaleID" = "femaleID")) %>% 
              pull(litterSeq)
}

create_id_mate.candidate <- function(femaleID, date, clan) {
  
  input <- tibble::tibble(femaleID, date, clan)
  
  input %>%
    select(.data$date, .data$clan) %>%
    arrange(.data$date, .data$clan) %>%
    distinct() %>%
    # need to group by here otherwise find_males will find all the males for each column
    group_by(.data$date, .data$clan) %>%
    mutate(candidateID = list(find_clan_id.male.selector(clan = .data$clan, at = .data$date))) %>% # candidates are selector males on the conception date
    ungroup() %>%
    tidyr::unnest(col = candidateID) %>%
    left_join(x = input, y = ., by = c("date", "clan")) -> output
  
  output
  
}
