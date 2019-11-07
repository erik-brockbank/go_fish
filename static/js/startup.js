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
    if (urlParams.has("mode") && urlParams.get("mode").includes("test")) {istest = true;}

    // Set experiment condition
    var control = Math.random() < 0.5;

    console.log("Starting experiment with test = ", istest, "; control = ", control);
    var exp = new Experiment(istest, control);
    // Initialize experiment with global variables used throughout experiment logic
    // NB: this is cumbersome but means we avoid referencing these as global constants inside the experiment logic
    exp.initialize(htmlpath = HTML_LOOKUP["experiment"],
                    inst_htmlpath = HTML_LOOKUP["instructions"],
                    evidence_htmlpath = HTML_LOOKUP["evidence"],
                    evidence_resp_htmlpath = HTML_LOOKUP["evidence_resp"],
                    prediction_htmlpath = HTML_LOOKUP["prediction"],
                    generate_htmlpath = HTML_LOOKUP["generate"],
                    judgment_htmlpath = HTML_LOOKUP["judgment"],
                    eval_htmlpath = HTML_LOOKUP["evaluation"],
                    memory_htmlpath = HTML_LOOKUP["memory"],
                    fish_img_path = IMAGE_LOOKUP["fish_img"],
                    no_fish_img_path = IMAGE_LOOKUP["no_fish_img"],
                    fish_img_path_small = IMAGE_LOOKUP["fish_checkbox"],
                    no_fish_img_path_small = IMAGE_LOOKUP["no_fish_checkbox"],
                    instruction_array = INSTRUCTION_ARRAY,
                    trial_array = TRIAL_ARRAY,
                    judgment_array = JUDGMENT_ARRAY,
                    eval_array = EVAL_ARRAY,
                    memory_array = MEMORY_ARRAY,
                    fish_caught_msg = OUTCOME_POSITIVE,
                    no_fish_msg = OUTCOME_NEGATIVE,
                    fish_explain_msg = EXPLAIN_RESPONSE_POS,
                    no_fish_explain_msg = EXPLAIN_RESPONSE_NEG,
                    fish_describe_msg = CONTROL_RESPONSE_POS,
                    no_fish_describe_msg = CONTROL_RESPONSE_NEG,
                    end_game_msg = ENDGAME_MSG);
    // Run experiment!
    exp.run();
};
