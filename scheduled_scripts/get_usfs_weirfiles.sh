#!/usr/bin/env bash

# Downloads the UNH/USFS loggernet files listed in weirfiles.txt into
# restricted_QAQC/data/unh_sensor_data/. Paths are resolved relative to this
# script, so the same script works on the server (/home/mike/shiny) and in a
# local clone (~/git/hbef/shiny).
#
# weirfiles.txt: one path per line, relative to loggernetfiles_complete/.
# Spaces are URL-encoded on request; the local filename is the line's basename
# as written (so "realtime_data/HB%20Mainstem_HB_mainstem.dat" is saved as
# "HB%20Mainstem_HB_mainstem.dat", which is what the R scripts expect).
#
# Notes:
#   - Use https. The server 302-redirects http to https, and credentials are
#     not reliably carried across that redirect (-> 401).
#   - The password is passed via a netrc on a file descriptor, so it never
#     appears on the command line (ps) or in logs.
#   - Each file is downloaded to a temp file and only moved into place on
#     success; a failed request no longer clobbers the last good copy with a
#     0-byte file.

set -u

script_dir="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" && pwd)"
dest_dir="$script_dir/../restricted_QAQC/data/unh_sensor_data"
pw_file="$script_dir/lognet_pw"
list_file="$script_dir/weirfiles.txt"
base_url='https://hbrsensor.sr.unh.edu/data/hbrloggernet/loggernetfiles_complete'

netrc() {
    printf 'machine hbrsensor.sr.unh.edu login hbrsensor password %s\n' "$(< "$pw_file")"
}

status=0
while IFS= read -r f || [[ -n "$f" ]]; do
    [[ -z "$f" || "$f" == \#* ]] && continue
    out="$dest_dir/$(basename "$f")"
    tmp="$out.part"
    if curl -sS --fail --retry 2 --netrc-file <(netrc) -o "$tmp" "$base_url/${f// /%20}"; then
        mv -f "$tmp" "$out"
    else
        echo "$(date '+%F %T') failed to retrieve $f" >&2
        rm -f "$tmp"
        status=1
    fi
done < "$list_file"

exit $status
