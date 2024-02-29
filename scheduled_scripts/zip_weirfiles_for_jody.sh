#!/usr/bin/env bash

cd /home/mike/data/send_to_jody/
cp -r /home/mike/shiny/restricted_QAQC/data/unh_sensor_data/weir?_Ws_* hbef_zq_data/
cp -r /home/mike/shiny/restricted_QAQC/data/unh_sensor_data/static_raw_weirdata/Weir_?.csv hbef_zq_data/
zip -r hbef_zq_data.zip hbef_zq_data/
