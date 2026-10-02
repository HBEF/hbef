# Handoff

## Sample archive portal (2026-10-02)

The archive investigation and next steps live in
`hbef_misc/archive_inventory/HANDOFF.md`. Running the merger is covered in
`scheduled_scripts/README.txt`.

What changed here:

- `scheduled_scripts/archive_merger.R` reads Amey's per-watershed xlsx files
  from `restricted_QAQC/data/archive_data/stream_updates/`, flags lab
  duplicate analyses in a `duplicate` column, and writes the portal data to
  the gitignored `HTML/archive_explore/archive_data.js`.
- `HTML/archive_explore/archive_explore.html` is now a template that loads
  that file.
- `restricted_QAQC/data/archive_data/` is gitignored. It was missing on the
  server and was copied over by hand on 2026-10-02. Copy it again after any
  server rebuild.
