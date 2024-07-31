library(tidyverse)
library(feather)
library(lubridate)

#TODO: HBK will be properly included in macrosheds after the 2025 run, so
# some of the HBK stuff below should be re-enabled.
# ...wait a minute. no. should it be considered a stream gauge site in ms?
# or are these Q data ever-provisional?

#TODO:
#see additional todos at bottom

#NOTES: also copy helpers.R (not global.R) [forreal? why risk it?]
#server/nSiteNVar_server.R and ui/nSiteNVar_ui.R have been modified to allow precip gauge viewing (and some little stuff)
#might have to copy css and js too. if so, remove biplot-related stuff from js
#note that map_server.R is slightly modified

setwd('~/git/hbef/shiny/watershed_exploration')

#copy macrosheds files into watershed_exploration dir ####

# file.copy(from = '../../../macrosheds/portal/data/general/site_data.csv',
#           to = 'data/general/site_data.csv',
#           overwrite = TRUE)
file.copy(from = '../../../macrosheds/portal/data/general/variables.csv',
          to = 'data/general/variables.csv',
          overwrite = TRUE)
# file.copy(from = '../../../macrosheds/portal/data/general/sites_with_discharge.csv',
#           to = 'data/general/sites_with_discharge.csv',
#           overwrite = TRUE)

for(p in c('discharge', 'precip_flux_inst_scaled', 'stream_chemistry',
           'stream_flux_inst_scaled', 'ws_boundary', 'precip_chemistry',
           'precipitation', 'stream_gauge_locations')){

    file.copy(from = paste0('../../../macrosheds/portal/data/hbef/', p),
              to = 'data/hbef/',
              recursive = TRUE,
              overwrite = TRUE)
}

#add mainstem site and w101 to site data file, and to list of sites with Q ####

# hbk_entry = tibble(
#     domain = 'hbef',
#     pretty_domain = 'Hubbard Brook',
#     network = 'lter',
#     pretty_network = 'LTER',
#     stream = 'Hubbard Brook',
#     site_code = 'HBK',
#     full_name = 'Hubbard Brook Mainstem',
#     site_type = 'stream_gauge',
#     latitude = 43.93974,
#     longitude = -71.70020,
#     CRS = 4326,
#     ws_area_ha = 3174.5,
#     in_workflow = 1,
#     notes = NA_character_,
#     local_time_zone = 'America/New_York',
#     includes_snow = NA_real_
# )

# ws101_entry = tibble(
#     domain = 'hbef',
#     pretty_domain = 'Hubbard Brook',
#     network = 'lter',
#     pretty_network = 'LTER',
#     stream = 'Stream 101',
#     site_code = 'w101',
#     full_name = 'Watershed 101',
#     site_type = 'stream_sampling_point',
#     latitude = 43.93593,
#     longitude = -71.73628,
#     CRS = 4326,
#     ws_area_ha = 12.1,
#     in_workflow = 1,
#     notes = NA_character_,
#     local_time_zone = 'America/New_York',
#     includes_snow = NA_real_
# )

# read_csv('data/general/site_data.csv') %>%
#     bind_rows(hbk_entry) %>% #, ws101_entry) %>%
#     filter(domain == 'hbef') %>%
#     distinct() %>%
#     write_csv('data/general/site_data.csv')
# 
# tibble(network = c('lter'),#, 'lter'),
#        domain = c('hbef'),#, 'hbef'),
#        site_code = c('HBK')) %>% #, 'w101')) %>%
#     bind_rows(read_csv('data/general/sites_with_discharge.csv')) %>%
#     distinct() %>%
#     write_csv('data/general/sites_with_discharge.csv')

#retrieve and munge raw mainstem Q; insert into filestructure ####

#modify last line of shell script to run locally
system('../scheduled_scripts/get_usfs_weirfiles_local.sh')

header = read_csv('../restricted_QAQC/data/unh_sensor_data/HB%20Mainstem_HB_mainstem.dat',
                  skip = 1,
                  col_names = FALSE,
                  n_max = 1)
mainstem = read_csv('../restricted_QAQC/data/unh_sensor_data/HB%20Mainstem_HB_mainstem.dat',
                    skip = 4,
                    col_names = FALSE)
colnames(mainstem) = header

mainstem %>%
    select(datetime = TIMESTAMP,
           val = Q) %>%
           # TempC = Sensor1tempC) %>%
    group_by(datetime = lubridate::floor_date(datetime, unit = 'day')) %>%
    summarize(val = mean(val, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(site_code = 'HBK',
           var = 'IS_discharge',
           ms_status = 0,
           ms_interp = 0,
           val_err = 0) %>%
    select(datetime, site_code, var, val, ms_status, ms_interp, val_err) %>%
    write_feather('data/hbef/discharge/HBK.feather')


#create the full HBEF spatial data collection ####

# w101 = sf::st_read('../../../macrosheds/portal/data/general/shed_boundary/shed_boundary.shp') %>%
#     as_tibble() %>%
#     filter(site_code == 'w101')

sited <- read_csv('data/general/site_data.csv') %>% 
    filter(domain == 'hbef',
           site_type == 'stream_gauge')
hbef_sheds <- sf::st_read('~/git/macrosheds/portal/data/general/shed_boundary/shed_boundary.shp') %>% 
    # as.data.frame() %>% 
    filter(site_code %in% sited$site_code)

#only re-enable this if you need to change sheds (like add w101 back in)
# sf::st_read('data/general/hbk_shed/hbef_wsheds.shp') %>%
#     sf::st_buffer(dist = 0.1) %>%
#     sf::st_union() %>%
#     sf::st_as_sf() %>%
#     sf::st_transform(4326) %>%
#     as_tibble() %>%
#     rename(geometry = x) %>%
#     mutate(site_code = 'HBK') %>%
#     bind_rows(hbef_sheds) %>%
#     # bind_rows(sf::st_read('data/general/shed_boundary/shed_boundary.shp')) %>%
#     # bind_rows(w101) %>%
#     distinct(site_code, .keep_all = TRUE) %>%
#     sf::st_as_sf() %>%
#     sf::st_write('data/general/shed_boundary/shed_boundary.shp',
#                  delete_layer = TRUE)

#insert rain gauge data into the filstructure as if gauges are watershed sites ####

# file.copy(from = '../../../macrosheds/data_acquisition/data/lter/hbef/munged/precipitation__13/')

#TODO:
#1. update the annual report pdf in www/ (if available)
#1.5	separate the first page with: `pdftk watershed_report_full.pdf cat 1-1 output watershed_report_page1.pdf`
#2. update most portal files through github
#3. update primary data via `rsync -avhPz hbef/ mike@165.22.183.247:shiny/watershed_exploration/data/hbef/`
#   This will only overwrite individual files that have updated, leaving intact old files, including gauge locs and stuff
#4. on server, run `sudo systemctl restart shiny-server.service`
