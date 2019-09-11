


Experiment = function(istest, control, htmlpath, inst_htmlpath, instruction_array, trial_array, eval_array) {
    this.istest = istest; // `test` experiments simulate a real experiment but write results as TEST_{exptid}.json
    this.control = control; // `control` holds condition: TRUE if control, else FALSE
    this.htmlpath = htmlpath; // path to html file to load for experiment
    this.inst_htmlpath = inst_htmlpath; // path to html file to load for instructions
    this.instruction_array = instruction_array; // array object used for instructions

    this.trialIndex = 0; // Index for keeping track of trial iterations
    this.evalIndex = 0; // Index for keeping track of rule evaluation iterations
    this.trialArray = trial_array; // array object used for trials
    this.evalArray = eval_array; // array object used for the evaluation task
};


Experiment.prototype.run = function() {
    var inst = new Instructions(this.inst_htmlpath, this.instruction_array);
    inst.run(this.startTrials.bind(this));
};

Experiment.prototype.startTrials = function() {
    console.log("Starting experiment trials");
    console.log(this);

    // Load html for running trials, fill in appropriate text
    var that = this;
    $("body").load(this.htmlpath, function() {
        that.showEvidence();
    });
};

Experiment.prototype.showEvidence = function() {
    console.log("Showing evidence for trial: ", this.trialIndex + 1);

    // Process trial object for this evidence trial
    var trialObj = this.trialArray[this.trialIndex];
    var outcomeText = "";
    if (trialObj.outcome == 1) {
        outcomeText = OUTCOME_POSITIVE; // TODO pass this in (or have separate constants file exp_constants or something)
    } else if (trialObj.outcome == 0) {
        outcomeText = OUTCOME_NEGATIVE; // TODO pass this in (or have separate constants file exp_constants or something)
    }

    // Display html for this evidence trial
    $("#exp-container").empty();
    $("#exp-container").load(HTML_LOOKUP["evidence"], function() { // TODO pass in html_lookup path
        $("#evidence-outcome").text(outcomeText);
    });

    // Update button response
    var that = this;
    $(".next-button").unbind().click(function() {
        // TODO process time to click here (did they read this page?) and add data to experiment trial object
        that.showEvidenceResponse();
    });
};

Experiment.prototype.showEvidenceResponse = function() {
    console.log("Collecting evidence response for trial: ", this.trialIndex + 1);

    // Process trial object for this evidence response trial
    var trialObj = this.trialArray[this.trialIndex];
    var responseBanner = "";
    if (this.control) {
        if (trialObj.outcome == 1) {
            responseBanner = CONTROL_RESPONSE_POS;
        } else if (trialObj.outcome == 0) {
            responseBanner = CONTROL_RESPONSE_NEG;
        }
    } else {
        if (trialObj.outcome == 1) {
            responseBanner = EXPLAIN_RESPONSE_POS;
        } else if (trialObj.outcome == 0) {
            responseBanner = EXPLAIN_RESPONSE_NEG;
        }
    }

    // Display html for this response trial
    $("#exp-container").empty();
    $("#exp-container").load(HTML_LOOKUP["evidence_resp"], function() { // TODO pass in html_lookup path
        $("#evidence-response-banner").text(responseBanner);
    });

    // Update button response
    var that = this;
    $(".next-button").unbind().click(function() {
        // TODO process whether they wrote anything here (prevent from clicking if they didn't write anything) and add what they wrote to experiment trial object
        that.showPrediction();
    });
};

Experiment.prototype.showPrediction = function() {
    console.log("Collecting prediction for trial: ", this.trialIndex + 1);
    // Process trial object for this prediction trial
    var trialObj = this.trialArray[this.trialIndex];


    // Display html for this prediction trial
    $("#exp-container").empty(); // TODO consider making a separate function to clear stuff out, we call this a lot...
    $("#exp-container").load(HTML_LOOKUP["prediction"], function() { // TODO pass in html_lookup path
        // TODO display prediction shape here
    });

    // Update button response
    this.trialIndex += 1;
    var that = this;
    $(".next-button").unbind().click(function() {
        // TODO process whether they clicked everything here (prevent from clicking if they didn't click stuff) and add their data to experiment trial object
        if (that.trialIndex >= that.trialArray.length) {
            console.log("Completed all trials.");
            that.showRuleGeneration();
        } else {
            that.showEvidence();
        }
    });
};

Experiment.prototype.showRuleGeneration = function() {
    console.log("Collecting rule generation.");

    // Display html for rule generation
    $("#obs-container").hide(); // TODO make separate function to clear out full trial stuff (observations etc.)
    $("#exp-container").empty();
    $("#exp-container").load(HTML_LOOKUP["generate"], function() {
        // TODO move the below in here potentially?
    });

    // Update button response
    var that = this;
    $(".next-button").unbind().click(function() {
        // TODO process whether they wrote anything here (prevent from clicking if they didn't write anything) and add what they wrote to experiment object
        that.showJudgmentTask();
    });
};

Experiment.prototype.showJudgmentTask = function() {
    console.log("Collecting rule judgments.");

    // Display html for rule judgment task
    $("#exp-container").empty();
    $("#exp-container").load(HTML_LOOKUP["judgment"], function() {
        // TODO fill in judgment stimuli
    });

    // Update button response
    var that = this;
    $(".next-button").unbind().click(function() {
        // TODO process whether they clicked anything here (prevent from clicking next if they didn't) and add what they selected to experiment object
        that.showEvaluationTask();
    });
};

Experiment.prototype.showEvaluationTask = function() {
    console.log("Showing rule evaluation for rule: ", this.evalIndex + 1);
    var ruleEval = this.evalArray[this.evalIndex];

    // Display html for evaluation task
    $("#exp-container").empty();
    $("#exp-container").load(HTML_LOOKUP["evaluation"], function() {
        $("#eval-rule").text(ruleEval.rule_text);
    });

    // Update button response
    this.evalIndex += 1;
    var that = this;
    $(".next-button").unbind().click(function() {
        // TODO process whether they clicked anything here (prevent from clicking next if they didn't) and add what they selected to experiment object
        if (that.evalIndex >= that.evalArray.length) {
            console.log("Completed all evaluations.");
            that.showMemoryTask();
        } else {
            that.showEvaluationTask();
        }
    });
};

Experiment.prototype.showMemoryTask = function() {
    console.log("Showing memory task.");

    // Display html for memory task
    $("#exp-container").empty();
    $("#exp-container").load(HTML_LOOKUP["memory"], function() {
        // TODO fill in memory items
    });

    // Update button response
    var that = this;
    $(".next-button").unbind().click(function() {
        // TODO process whether they clicked anything here (prevent from clicking next if they didn't) and add what they selected to experiment object
        that.endExperiment();
    });
};

Experiment.prototype.endExperiment = function() {
    console.log("End of experiment!");

    $("#exp-container").empty();
    $(".next-button").hide();
    $("#exp-container").text("All done! Thanks for playing!"); // TODO make this a global, add in <h1> formatting

    // TODO write results to json!
};
