


Experiment = function(istest, control, htmlpath, inst_htmlpath, instruction_array, trial_array) {
    this.istest = istest; // `test` experiments simulate a real experiment but write results as TEST_{exptid}.json
    this.control = control; // `control` holds condition: TRUE if control, else FALSE
    this.htmlpath = htmlpath; // path to html file to load for experiment
    this.inst_htmlpath = inst_htmlpath; // path to html file to load for instructions
    this.instruction_array = instruction_array; // array object used for instructions

    this.trialIndex = 0; // Index for keeping track of trial iterations
    this.trialArray = trial_array; // array object used for trials
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
    var trialObj = this.trialArray[this.trialIndex];

    // Process trialObj for this evidence trial
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
    var trialObj = this.trialArray[this.trialIndex];

    // Process trialObj for this evidence response trial
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
    var trialObj = this.trialArray[this.trialIndex];

    // Process trialObj for this prediction trial


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
    $("#exp-container").load(HTML_LOOKUP["generate"]);

    // Update button response
    var that = this;
    $(".next-button").unbind().click(function() {
        // TODO process whether they wrote anything here (prevent from clicking if they didn't write anything) and add what they wrote to experiment object
        that.showJudgmentTask();
    });
};

Experiment.prototype.showJudgmentTask = function() {
    console.log("Collecting rule judgments.");

};
