
prep_cfl_data <- function() {
  ## READ IN THE MAPLIGHT DATA
  
  pos_all <- read_csv(here("data-raw", "Polarized_Pluralism_Replication", "Data", "all_positions_including114-csv.csv"))
  
  ## READ IN THE ROLL CALL DATA, VOTES
  
  # bwilden - getting rollcall vote data with codes for bill subjects
  # rollcalls <- read_csv(here("data-raw", "Polarized_Pluralism_Replication", "Data", "HSall_rollcalls.csv"))
  rollcalls <- jsonlite::fromJSON("https://voteview.com/static/data/out/rollcalls/HSall_rollcalls.json", flatten = TRUE)
  
  votes <- read_csv(here("data-raw", "Polarized_Pluralism_Replication", "Data", "HSall_votes.csv"))
  
  ## READ IN BILL-SPECIFIC INFORMATION FROM CONGRESSIONAL BILLS PROJECT
  
  cbp <- read_csv(here("data-raw", "Polarized_Pluralism_Replication", "Data", "bills93-114 2.csv"))
  
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
    right_join(pos_all_edges, by = "BillID")
  
  # bwilden - Renamed data
  legislators <- rollcalls
  
  return(lst(groups, legislators))
}

expand_group_dispositions <- function(groups_df, 
                                      congress,
                                      n_groups,
                                      n_bills) {
  groups_df <- groups_df %>% 
    filter(str_starts(BillID, congress))
  
  top_groups <- groups_df %>% 
    group_by(orgname) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    slice(1:n_groups) %>% 
    pull(orgname)
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
                select(orgname, business) %>% 
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
    select(y_ij = disposition, 
           group_id = orgname, 
           bill_id = BillID,
           business, rep) %>% 
    # Remove groups that abstained on every bill
    group_by(group_id) %>% 
    mutate(total_2s = sum(y_ij == 2)) %>% 
    ungroup() %>% 
    filter(total_2s != n_bills) %>% 
    # Bills need at least 5 votes
    group_by(bill_id) %>% 
    mutate(total_2s_bill = sum(y_ij == 2)) %>% 
    ungroup() %>% 
    filter(total_2s_bill < n_groups - 5) %>% 
    # Remove problematic periods
    mutate(group_id = str_remove_all(group_id, "\\."))
  
  # Create binary data set
  ij_obs <- ij_all %>% 
    filter(y_ij != 2) %>% 
    mutate(y_ij = if_else(y_ij == 3, 1, 0))
  
  ij_obs_wide <- ij_obs %>% 
    rename(yea = y_ij) %>% 
    select(group_id, bill_id, yea) %>% 
    pivot_wider(id_cols = group_id,
                values_from = yea,
                names_from = bill_id)
  
  ij_obs_rc <- ij_obs_wide %>% 
    select(-group_id) %>% 
    pscl::rollcall(legis.names = ij_obs_wide$group_id)
  
  return(lst(ij_all, ij_obs, ij_obs_rc))
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
