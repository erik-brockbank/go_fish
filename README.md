# go_fish

## To run experiment locallly:
1. cd to `go_fish` directory
2. `python -m http.server {port}` (or your favorite local web server)
3. In browser, navigate to:
    `http://localhost:{port}/go_fish.html` for experiment version
    `http://localhost:{port}/go_fish.html?&mode=test` for test version (writes a file prepended with `TEST_...` for easy debugging)

## Data for participants is in the /data folder (no identifying information was captured)
- Includes data for 17 participants in the first round of testing and 69 participants in the second round

## Analysis of existing data is in the /analysis folder
- `cogsci_analysis.R` contains all existing analysis for cog sci 2020 manuscript


