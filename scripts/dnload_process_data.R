# Purpose: download and process data
# Authour: Ryan N. Kinzer
# Modified: 5/31/19
# Updated: 6/16/2025

library(dplyr)

spp <- 'Chinook'
yr <- '2025'

# Get site config data
load('data/site_config_LGR_20250416.rda')

# need to order factor levels of nodes and sites
# 
site_loc <- crb_sites_sf

site_order <- PITcleanr::buildNodeOrder(parent_child)
saveRDS(site_order, file = './data/node_order.rds')
saveRDS(parent_child, file = './data/parent_child.rds')

pc_nodes = parent_child %>% 
  PITcleanr::addParentChildNodes(.,  configuration = configuration)

saveRDS(pc_nodes, file = './data/pc_nodes.rds')


site_grp <- site_order %>%
   mutate(grp = stringr::str_split(path, ' ', simplify = TRUE)[,2],
          grp = ifelse(grp == '', 'GRA', grp))

site_grp$grp <- sapply(site_grp$grp, switch,
  'GRA' = 'LGR',
  'ACM' = 'Asotin Creek',
  'ALPOWC' = 'Asotin Creek',
  'BRC' = 'Middle Fork',
  'CLC' = 'SF Clearwater',
  'COC' = 'Imnaha River',
  'CRC' = 'Lemhi River',
  'DWL' = 'Hatcheries',
  'GRS' = 'Downstream LGR',
  'IR1' = 'Imnaha River',
  'JA1' = 'Jacks Creek',
  'JOC' = 'Joseph Creek',
  'JUL' = 'Potlatch Creek',
  'LAP' = 'Lapwai Creek',
  'LC1' = 'Lolo Creek',
  'LLR' = 'Lemhi River',
  'LOOH' = 'Lookingglass',
  'LRL' = 'Lochsa River',
  'MAR' = 'Middle Fork',
  'NFS' = 'NF Salmon',
  'NPTH' = 'Hatcheries',
  'OXBO' = 'Hatcheries',
  'PCA' = 'Panther Creek',
  'RAPH' = 'Rapid River',
  'SC1' = 'SF Clearwater',
  'SFG' = 'SF Salmon',
  'SW1' = 'Selway River',
  'TAY' = 'Middle Fork',
  'TENMC2' = 'Asotin Creek',
  'UGR' = 'Grande Ronde',
  'USE' = 'Salmon River',
  'WB1' = 'White Bird',
  'WEN' = 'Wenaha',
  'WR1' = 'Wallowa')

configuration <- left_join(configuration, site_grp,
                 by = c('site_code' = 'node'))

site_vec <- site_grp %>%
  arrange(node_order, node) %>%
  pull(node)

node_order <- configuration %>%
   select(node, node_order) %>%
   filter(!is.na(node_order)) %>%
   distinct()

node_vec <- node_order %>%
  arrange(node_order, node) %>%
  pull(node) %>%
  unique()

# Get Dart obs for most recent year and add to previous years.

 
tmp_ls <- PITcleanr::compressDART(species = spp,
                        loc = 'GRA',
                        spawn_year = yr,
                        configuration = configuration)
 
tmp_mark <- tmp_ls$dart_obs %>%
  select(tag_code, mark_date, file_id, mark_site, rel_site, rel_date,
         mark_rear_type_name, mark_species_name, t_run, length, trans_status,
         trans_proj, trans_year) %>%
  distinct()

tmp_compress <- tmp_ls$compress_obs

dat_all <<- tmp_compress %>%
  mutate(site_code = str_split(node, '_', simplify = TRUE)[,1]) %>%
  left_join(node_order, by = 'node') %>%
  left_join(site_grp %>% select(-node_order), by = c('site_code' = 'node')) %>%
  left_join(site_loc, by = 'site_code') %>%
  mutate(node = fct_relevel(node, node_vec),
         site_code = fct_relevel(site_code, site_vec),
         grp = as.character(grp)) %>%
  left_join(tmp_mark, by = 'tag_code') %>%
  mutate(species = spp,
         spawn_yr = yr) %>%
  ungroup()

save(dat_all, file = './data/dat_all.rda')


#load('data/all_dart_obs.rda')
#pit_exp <- read_csv('data/pit_expansion_2020.csv')

# all_dart_obs <- all_dart_obs %>%
#   left_join(site_loc, by = 'SiteID') %>%
#   mutate(Node = fct_relevel(Node, node_vec),
#          SiteID = fct_relevel(SiteID, site_vec),
#          Group = as.character(Group))

# Set amazon pass codes

#setKeys()