#!/usr/bin/env bash
# kept for watershed_exploration/update_data.R; get_usfs_weirfiles.sh now
# resolves its paths relative to itself, so it works locally as-is.
exec "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/get_usfs_weirfiles.sh" "$@"
