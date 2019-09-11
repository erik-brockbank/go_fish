/*
 * Startup library for loading in html content
 * Instructions for startup:
 * 1. navigate to go_fish directory
 * 2. python -m http.server {port}
 * 3. navigate to http://localhost:8000/index.html for experiment version
 *      http://localhost:8000/index.html?&mode=test for test version
 */

$(window).ready(function() {
    $("body").load(HTML_LOOKUP["consent"], function() {
        $("#consent-button").click(loadExperiment);
    });
});

loadExperiment = function() {
    console.log("Consent form agree.");
    // URL parsing
    // ex. http://localhost:8000/index.html?&mode=test
    var istest = false;
    var urlParams = new URLSearchParams(window.location.href);
    if (urlParams.has("mode") & urlParams.get("mode") == "test") {istest = true;}

    // Set experiment condition
    var control = Math.random() < 0.5;

    console.log("Starting experiment with test = ", istest, "; control = ", control);
    var exp = new Experiment(istest, control, HTML_LOOKUP["experiment"], HTML_LOOKUP["instructions"],
                                INSTRUCTION_ARRAY, TRIAL_ARRAY);
    exp.run();
};
