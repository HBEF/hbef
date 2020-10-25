parse_sensor_data.R is sorta derelict. at some point in prehistory, we received a file called CR1000_HBF_WQual_W3.csv, a compilation of data from various UNH sensors in watershed 3. parse_sensor_data.R has been replaced by process_unh_data.R, which handles raw, updating sensor files from watersheds 3 and 9.

get_usfs_weirfiles.sh runs periodically as a cron job. the rest of the updating weir files arrive as part of an rsync (also a cron job on this machine)
