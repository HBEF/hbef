parse_sensor_data.R is sorta derelict. at some point in prehistory, we received a file called CR1000_HBF_WQual_W3.csv, a compilation of data from various UNH sensors in watershed 3. parse_sensor_data.R has been replaced by process_unh_data.R, which handles raw, updating sensor files from watersheds 3 and 9.

get_usfs_weirfiles.sh runs periodically as a cron job. the rest of the updating weir files arrive as part of an rsync (also a cron job on this machine)

process_unh_data.R and process_S.CAN_data.R must run as cron tasks. process_unh_data.R must run first, because it begins by dropping the sensor4 table that they both use. the easiest way to ensure this is to source the latter from the former. so, that's what you'll find is happening. if you are rebuilding this server, you may have to update the path being sourced
