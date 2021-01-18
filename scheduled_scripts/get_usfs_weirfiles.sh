#!/bin/bash

while read l; do
wget -O "../restricted_QAQC/data/unh_sensor_data/$l"  "http://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete/realtime_data/$l" --user="hbrsensor" --password="kineo93"
done < /home/mike/shiny/scheduled_scripts/weirfiles.txt
#done < /home/mike/git/hbef/shiny/scheduled_scripts/weirfiles.txt
