
prep_cfl_data <- function(cfl_group_info_df) {
  ## READ IN THE MAPLIGHT DATA
  
  pos_all <- read_csv(here("data-raw", "all_positions_including114-csv.csv"))
  
  ## READ IN THE ROLL CALL DATA, VOTES
  
  # bwilden - getting rollcall vote data with codes for bill subjects
  # rollcalls <- read_csv(here("data-raw", "Polarized_Pluralism_Replication", "Data", "HSall_rollcalls.csv"))
  rollcalls <- jsonlite::fromJSON("https://voteview.com/static/data/out/rollcalls/HSall_rollcalls.json", 
                                  flatten = TRUE)
  
  votes <- read_csv(here("data-raw", "HSall_votes.csv"))
  
  ## READ IN BILL-SPECIFIC INFORMATION FROM CONGRESSIONAL BILLS PROJECT
  
  cbp <- read_csv(here("data-raw", "bills93-114 2.csv"))
  
  #standardize bill types in CBP to match Maplight ids
  pos_all$prefix[pos_all$prefix == "S"] <- "s"
  pos_all$prefix[pos_all$prefix == "H"] <- "hr"
  pos_all$prefix[pos_all$prefix == "HR"] <- "hres"
  pos_all$prefix[pos_all$prefix == "HC"] <- "hconres"
  pos_all$prefix[pos_all$prefix == "SR"] <- "sres"
  pos_all$prefix[pos_all$prefix == "SJ"] <- "sjres"
  pos_all$prefix[pos_all$prefix == "HJ"] <- "hjres"
  pos_all$prefix[pos_all$prefix == "SC"] <- "sconres"
  
  # bwilden - added Party
  cbp_select <- cbp %>% 
    dplyr::select(BillID, BillNum, BillType, Chamber, Cong, IntrDate, Major, Party)
  
  #join to Maplight data
  pos_all <- pos_all %>% 
    left_join(cbp_select, c("number" = "BillNum", "prefix" = "BillType", "session" = "Cong"))
  pos_all_edges <- pos_all %>% 
    dplyr::select(orgname, BillID, disposition, Party, grouptype)
  pos_all_edges <- pos_all_edges %>% 
    filter(complete.cases(pos_all_edges)) |> 
    # bwilden - Added business group dummy
    mutate(business = if_else(str_starts(grouptype, "G"), 1, 0))
  
  ## JOIN DATA TOGETHER
  
  rollcalls <- filter(rollcalls, congress > 108)
  votes <- filter(votes, congress > 108)
  
  votes$disposition <- NA
  votes$disposition[votes$cast_code == 1] <- "support"
  votes$disposition[votes$cast_code == 6] <- "oppose"
  # bwilden - Added Abstains
  votes$disposition[votes$cast_code %in% c(7, 8, 9)] <- "abstain"
  
  votes <- votes %>% filter(!is.na(disposition))
  
  rollcalls$bill_type <- gsub("[0-9]", "", rollcalls$bill_number) 
  rollcalls$bill_num <- gsub("[^0-9]", "", rollcalls$bill_number)
  rollcalls$BillID <- paste(rollcalls$congress, rollcalls$bill_type, rollcalls$bill_num, sep ="-")
  rollcalls <- dplyr::select(rollcalls, congress, chamber, rollnumber, BillID, date,
                             # bwilden - Added bill subject codes
                             issue_codes, crs_subjects, peltzman_codes, crs_policy_area)
  
  #merge by last position taken, last roll call taken
  last_rolls <- rollcalls %>% group_by(BillID) %>% summarise(lastrolldate = max(date))
  
  rollcalls <- rollcalls %>% left_join(last_rolls)
  rollcalls <- rollcalls %>% filter(date == lastrolldate)
  rollcalls <- rollcalls %>% filter(BillID %in% unique(pos_all_edges$BillID))
  final_rollnum <- rollcalls %>% group_by(BillID) %>% summarise(last_rollnum = max(rollnumber))
  rollcalls <- rollcalls %>% left_join(final_rollnum)
  rollcalls <- rollcalls %>% filter(rollnumber == last_rollnum)
  
  rollcalls <- rollcalls %>% left_join(votes)
  
  # bwilden - Added merge with cpb_select for Party variable
  rollcalls <- rollcalls %>% 
    left_join(cbp_select, by = "BillID") %>% 
    mutate(icpsr = as.character(icpsr)) %>% 
    select(icpsr, disposition, BillID, Party, issue_codes, crs_subjects, peltzman_codes, crs_policy_area)
  
  # bwilden - Adding bill subject info to group data
  groups <- rollcalls |> 
    select(BillID, issue_codes, crs_subjects, peltzman_codes, crs_policy_area) |> 
    distinct() |> 
    right_join(pos_all_edges, by = "BillID") |> 
  # bwilden - merging in group types
    left_join(cfl_group_info_df, by = "orgname") |> 
    mutate(group_id = str_remove_all(orgname, "\\."))
  
  # bwilden - Renamed data
  legislators <- rollcalls
  
  return(lst(groups, legislators))
}

find_top_groups <- function(input_df, type_category, top_n) {
  top_groups <- input_df |> 
    group_by(!!sym(type_category), orgname) |> 
    tally() |> 
    arrange(!!sym(type_category), -n) |> 
    group_by(!!sym(type_category)) |> 
    slice_head(n = top_n) |> 
    pull(orgname)
  return(top_groups)
}

expand_group_dispositions <- function(groups_df, 
                                      congress,
                                      n_groups = NULL,
                                      n_bills,
                                      ...) {
  groups_df <- groups_df %>% 
    filter(str_starts(BillID, congress))
  
  # top_sector <- find_top_groups(groups_df, "Sector", ...)
  # top_group_type <- find_top_groups(groups_df, "usecode", ...)
  # 
  # top_groups <- c(top_sector, top_group_type) |> unique()
  
  top_groups <- groups_df %>%
    group_by(orgname) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    slice(1:n_groups) %>%
    pull(orgname) |>
    unique()
  
  top_bills <- groups_df %>% 
    group_by(BillID) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    slice(1:n_bills) %>% 
    pull(BillID)
  
  ij_all <- tibble(
    BillID = top_bills
  ) %>% 
    tidyr::crossing(
      place_holder_disposition = c("support", "abstain", "oppose"),
      orgname = top_groups
    ) %>% 
    left_join(groups_df %>% 
                select(BillID, orgname, disposition)) %>% 
    mutate(disposition = if_else(is.na(disposition), "abstain", disposition)) %>% 
    select(-place_holder_disposition) %>% 
    left_join(groups_df %>% 
                select(BillID, Party) %>% 
                distinct()) %>% 
    left_join(groups_df %>% 
                select(orgname, business, usecode, Catname, Industry, Sector) %>% 
                distinct()) %>% 
    distinct() %>% 
    # If a business is ever a business it's a business
    group_by(orgname) %>% 
    mutate(business = if_else(sum(business) > 0, 1, 0)) %>% 
    ungroup() %>% 
    distinct() %>% 
    mutate(disposition = case_when(disposition == "oppose" ~ 1,
                                   disposition == "abstain" ~ 2,
                                   disposition == "support" ~ 3),
           rep = if_else(Party == 200, 1, 0)) %>% 
    select(position = disposition, 
           group_id = orgname, 
           bill_id = BillID,
           business, rep, usecode, Catname, Industry, Sector) %>% 
    # Remove groups that abstained on every bill
    group_by(group_id) %>% 
    mutate(total_2s = sum(position == 2)) %>% 
    ungroup() %>% 
    filter(total_2s != n_bills) %>% 
    # Bills need at least 5 votes
    group_by(bill_id) %>% 
    mutate(total_2s_bill = sum(position == 2)) %>% 
    ungroup() %>% 
    filter(total_2s_bill < length(top_groups) - 5) %>% 
    # Remove problematic period characters
    mutate(group_id = str_remove_all(group_id, "\\."))
  
  # Create binary data set
  ij_obs <- ij_all %>% 
    filter(position != 2) %>% 
    mutate(position = if_else(position == 3, 1, 0))
  
  ij_obs_wide <- ij_obs %>% 
    rename(yea = position) %>% 
    select(group_id, bill_id, yea) %>% 
    pivot_wider(id_cols = group_id,
                values_from = yea,
                names_from = bill_id)
  
  ij_obs_rc <- ij_obs_wide %>% 
    select(-group_id) %>% 
    pscl::rollcall(legis.names = ij_obs_wide$group_id)
  
  return(lst(ij_all, ij_obs, ij_obs_rc))
}


get_cfl_group_info <- function(cfl_posteriors) {
  groups_info <- read_csv(cfl_posteriors) |> 
    filter(respondent_type == "Organization") |> 
    select(orgname, usecode, Catname, Industry, Sector, Sector.Long) |> 
    distinct() |> 
    mutate(group_id = str_remove_all(orgname, "\\."))
  
  return(groups_info)
}


# tar_load(cfl_exp_data)
# cfl_exp_data$ij_all
# cfl_exp_data$ij_obs
# cfl_exp_data$ij_obs_rc
# x <- pscl::ideal(ij_obs_rc,
#       thin = 10,
#       dropList = list(lop = NA),
#       normalize = TRUE)
# 
# ij_obs %>% 
#   rename(yea = y_ij) %>% 
#   select(group_id, bill_id, yea) %>% 
#   pivot_wider(id_cols = group_id,
#               values_from = yea,
#               names_from = bill_id) %>% 
#   select(where(function(x) sum(is.na(x)) > 90))
# 
# dropRollCall(ij_obs_rc, dropList = list(lop = NA))
