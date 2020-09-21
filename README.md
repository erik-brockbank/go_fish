# go_fish

## To run experiment locallly:
1. cd to `go_fish` directory
2. set `PROD` global in `/static/js/startup.js`L11 to be either "test" or "prod"
3. `python -m http.server {port}` (or your favorite local web server)
4. In browser, navigate to:
- `http://localhost:{port}/go_fish.html` for experiment version
- `http://localhost:{port}/go_fish.html?&mode=test` for test version (writes a file prepended with `TEST_...` for easy debugging)

## Data for participants is in the /data folder (no identifying information was captured)
- Includes data for 17 participants in the first round of testing and 69 participants in the second round

## Analysis of existing data is in the /analysis folder
- `manuscript_analysis.R` contains all analyses for cog sci 2020 and subsequent manuscript.
- Coded correctness values for participant free response answers (hypothesis generation task) are in `free_response_coder1.csv` and `free_response_coder2.csv`. The two coded responses are combined with final values selected in `free_response_combined.csv`.


