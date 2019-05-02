/*
 * Startup library for loading in html content
 */

$(window).ready(function() {
    $("body").load("consent.html");
});

clickConsent = function() {
    console.log("ty next");
    loadExperiment();
}

loadExperiment = function() {
    // URL parsing
    // ex. http://localhost:8000/exp.html?&mode=test
    var istest = false;
    var urlParams = new URLSearchParams(window.location.href);
    if (urlParams.has("mode") & urlParams.get("mode") == "test") {istest = true;}

    // Set experiment condition
    var control = Math.random() < 0.5;

    console.log("starting experiment with test = ", istest, "; control = ", control);
    var exp = new Experiment(istest, control);
    exp.run();
}
