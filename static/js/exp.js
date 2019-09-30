


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
    var outcomeImg = "";
    if (trialObj.outcome == 1) {
        outcomeText = OUTCOME_POSITIVE; // TODO pass this in (or have separate constants file exp_constants or something)
        outcomeImg = "/img/fish_icon.png"; // TODO save this somewhere as a constant
    } else if (trialObj.outcome == 0) {
        outcomeText = OUTCOME_NEGATIVE; // TODO pass this in (or have separate constants file exp_constants or something)
        outcomeImg = "/img/no-fish_icon.png"; // TODO save this somewhere as a constant
    }


    // Display html for this evidence trial
    $("#exp-container").empty();
    $("#exp-container").load(HTML_LOOKUP["evidence"], function() { // TODO pass in html_lookup path
        $("#evidence-outcome").text(outcomeText);
        $("#evidence-outcome-img-container").html("<img class='evidence-outcome-img' src='" + outcomeImg + "' />");

        // TODO replace this with results of shape drawing process above
        // $("#evidence-shape-container").html("<img class='evidence-shape-img' src='" + evidenceShape + "' />");
        shapeInfo = trialObj.evidence;
        evidenceShape = new Lure(shapeInfo.top_shape, shapeInfo.bottom_shape, shapeInfo.top_color, shapeInfo.bottom_color);
        evidenceShape.drawLure(canvasId = "evidence-shape-canvas"); // TODO store this ID somewhere sensible
    });

    // Update button response
    var that = this;
    $("#next-exp").unbind().click(function() {
        // TODO process time to click here (did they read this page?) and add data to experiment trial object
        that.showEvidenceResponse();
    });
};


Experiment.prototype.showEvidenceResponse = function() {
    console.log("Collecting evidence response for trial: ", this.trialIndex + 1);

    // Process trial object for this evidence response trial
    var trialObj = this.trialArray[this.trialIndex];
    var responseBanner = "";
    var evidenceOutcomeImg = "";
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

    // TODO consolidate this with the above
    // TODO make same-size fish and no-fish images for outcomes and checkboxes
    if (trialObj.outcome == 1) {
        evidenceOutcomeImg = "/img/fish_icon-checkbox.png";
    } else {
        evidenceOutcomeImg = "/img/no-fish_icon-checkbox.png";
    }

    evidenceShape = "/img/lure_dummy.png"; // TODO replace this with actual retrieval of this trial's shape

    // Display html for this response trial
    var that = this;
    $("#exp-container").empty();
    $("#exp-container").load(HTML_LOOKUP["evidence_resp"], function() { // TODO pass in html_lookup path
        $("#evidence-response-banner").text(responseBanner);
        $("#evidence-response-banner").css("font-style", "Italic");
        // TODO replace this with a function that handles observed evidence updates
        $("#obs-item-" + (that.trialIndex + 1)).html("<img class='obs-item-img' src='" + evidenceShape + "' />");
        $("#obs-outcome-" + (that.trialIndex + 1)).html("<img class='obs-outcome-img' src='" + evidenceOutcomeImg + "' />");
        if (trialObj.outcome == 1) {
            $("#obs-item-" + (that.trialIndex + 1)).css("border", "5px solid black"); // draw bold box around fish catches
            $("#obs-item-" + (that.trialIndex + 1)).css("margin", "5px"); // decrease margin to keep everything lined up
        }
    });

    // Update button response
    var that = this;
    $("#next-exp").unbind().click(function() {
        // TODO process whether they wrote anything here (prevent from clicking if they didn't write anything) and add what they wrote to experiment trial object
        that.showPrediction();
    });
};


Experiment.prototype.showPrediction = function() {
    console.log("Collecting prediction for trial: ", this.trialIndex + 1);
    // Process trial object for this prediction trial
    var trialObj = this.trialArray[this.trialIndex];

    var predictionShape = "/img/lure_dummy.png"; // TODO get actual prediction shape info here

    // Display html for this prediction trial
    $("#exp-container").empty(); // TODO consider making a separate function to clear stuff out, we call this a lot...
    $("#exp-container").load(HTML_LOOKUP["prediction"], function() { // TODO pass in html_lookup path
        // TODO replace this with results of actual shape prediction process above
        $("#prediction-img-container").html("<img class='prediction-shape-img' src='" + predictionShape + "' />");
    });

    // Update button response
    this.trialIndex += 1;
    var that = this;
    $("#next-exp").unbind().click(function() {
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
    $("#next-exp").unbind().click(function() {
        // TODO process whether they wrote anything here (prevent from clicking if they didn't write anything) and add what they wrote to experiment object
        that.showJudgmentTask();
    });
};


Experiment.prototype.showJudgmentTask = function() {
    console.log("Collecting rule judgments.");

    // Display html for rule judgment task
    $("#exp-container").empty();
    $("#exp-container").load(HTML_LOOKUP["judgment"], function() {
        for (genIndex = 1; genIndex <= GENERATE_ARRAY.length; genIndex++) {
            // TODO replace the below with results of actual generation shape process (get elem in GENERATE_ARRAY at genIndex, do stuff)
            var genShape = "/img/lure_dummy.png"; // TODO get actual shape info here
            $("#generate-img-container-" + genIndex).html(
                "<img class='generate-img' id='generate-img-" + genIndex + "' src='" + genShape + "' />");
        }
    });

    // Update button response
    var that = this;
    $("#next-exp").unbind().click(function() {
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
        $("#obs-container").show(); // show observed evidence during evaluation task
        $("#eval-rule").text(ruleEval.rule_text);
    });

    // Update button response
    this.evalIndex += 1;
    var that = this;
    $("#next-exp").unbind().click(function() {
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
    $("#obs-container").hide(); // TODO make separate function to clear out full trial stuff (observations etc.)
    $("#exp-container").load(HTML_LOOKUP["memory"], function() {
        for (memIndex = 1; memIndex <= MEMORY_ARRAY.length; memIndex++) {
            // TODO replace the below with results of actual memory shape process (get elem in MEMORY_ARRAY at memIndex, do stuff)
            var predictionShape = "/img/lure_dummy.png"; // TODO get actual memory shape info here
            $("#memory-img-container-" + memIndex).html(
                "<img class='memory-probe-img' id='memory-probe-img-" + memIndex + "' src='" + predictionShape + "' />");
        }
    });

    // Update button response
    var that = this;
    $("#next-exp").unbind().click(function() {
        // TODO process whether they clicked anything here (prevent from clicking next if they didn't) and add what they selected to experiment object
        that.endExperiment();
    });
};


Experiment.prototype.endExperiment = function() {
    console.log("End of experiment!");

    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#exp-container").html("<h1>All done! Thanks for playing!</h1>"); // TODO make this a global

    // TODO write results to json!
};
