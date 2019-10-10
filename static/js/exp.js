
/*
 * TODO
 * Experimental
 * - correct shapes in prediction, judgment, memory
 *
 * Core logic
 * - write to json
 * - validate inputs
 *
 * Cleanup
 * - clean up html (try referencing globals within html)
 * - comment css?
 * - try to separate out html-referencing functions in this file (if it knows about e.g. an object's id, quarantine it)
 */





/*
 * Experiment class used to run core experiment logic. Note instantiating the class below does
 * very little initialization. Instead, the initialize() function handles the nitty gritty of
 * assigning various global states.
 */
Experiment = function(istest, control) {
    this.istest = istest; // `test` experiments simulate a real experiment but write results as TEST_{exptid}.json
    this.control = control; // `control` holds condition: TRUE if control, else FALSE
    this.data = {}; // object for storing data that we write to json at end of experiment
};


/*
 * Initialization function for Experiment class. To keep class instantiation clean, this function
 * does all the kitchen sink logic of assigning relevant html and image paths and other global state
 * that sub-functions below don't need to know about
 */
Experiment.prototype.initialize = function(htmlpath, inst_htmlpath, evidence_htmlpath, evidence_resp_htmlpath,
        prediction_htmlpath, generate_htmlpath, judgment_htmlpath, eval_htmlpath, memory_htmlpath,
        fish_img_path, no_fish_img_path, fish_img_path_small, no_fish_img_path_small,
        instruction_array, trial_array, judgment_array, eval_array, memory_array,
        fish_caught_msg, no_fish_msg, fish_explain_msg, no_fish_explain_msg,
        fish_describe_msg, no_fish_describe_msg, end_game_msg) {
    // file path variables
    this.htmlpath = htmlpath; // path to html file to load for experiment
    this.inst_htmlpath = inst_htmlpath; // path to html file to load for instructions
    this.evidence_htmlpath = evidence_htmlpath; // path to html file for displaying evidence
    this.evidence_resp_htmlpath = evidence_resp_htmlpath; // path to html file for responding to evidence
    this.prediction_htmlpath = prediction_htmlpath; // path to html file for collecting prediction data
    this.generate_htmlpath = generate_htmlpath; // path to html file for hypothesis generation task
    this.judgment_htmlpath = judgment_htmlpath; // path to html file for judgment task
    this.eval_htmlpath = eval_htmlpath; // path to html file for evaluation task
    this.memory_htmlpath = memory_htmlpath; // path to html file for memory task
    this.fish_img_path = fish_img_path; // path to image used when fish caught
    this.no_fish_img_path = no_fish_img_path; // path to image used when no fish caught
    this.fish_img_path_small = fish_img_path_small; // path to small image of fish caught used for checkboxes, etc.
    this.no_fish_img_path_small = no_fish_img_path_small; // path to small image of no fish used for checkboxes, etc.

    // global objects
    this.instruction_array = instruction_array; // array object with text and images used for instructions
    this.trial_array = trial_array; // array object with lures used for trials
    this.judgment_array = judgment_array; // array object with lures used for judgment task
    this.evalArray = eval_array; // array object with rules used in the evaluation task
    this.memory_array = memory_array; // array object with lures used in the memory task

    // global text variables
    // (wording of messages displayed throughout experiment that we save as constants rather than edit within the js below)
    this.fish_caught_msg = fish_caught_msg; // message displayed to users when a fish was caught
    this.no_fish_msg = no_fish_msg; // message displayed to users when no fish was caught
    this.fish_explain_msg = fish_explain_msg; // message displayed to users prompting them to explain why a fish was caught with a given lure
    this.no_fish_explain_msg = no_fish_explain_msg; // message displayed to users prompting them to explain why a fish was *not* caught with a given lure
    this.fish_describe_msg = fish_describe_msg; // message displayed to users prompting them to describe the lure when a fish was caught
    this.no_fish_describe_msg = no_fish_describe_msg; // message displayed to users prompting them to describe the lure when a fish was *not* caught
    this.end_game_msg = end_game_msg; // message displayed to users at the end of the game

    // global state variables
    this.trial_index = 0; // Index for keeping track of trial iterations
    this.eval_index = 0; // Index for keeping track of rule evaluation iterations
};


Experiment.prototype.run = function() {
    var inst = new Instructions(this.inst_htmlpath, this.instruction_array);
    this.data["instruction_data"] = {};
    this.instruction_start_ts = new Date().getTime();
    inst.run(this.startTrials.bind(this));
};


Experiment.prototype.startTrials = function() {
    console.log("Starting experiment trials");
    // write to data object
    this.data["instruction_data"]["instruction_time"] = new Date().getTime() - this.instruction_start_ts;
    this.data["trial_data"] = [];

    // Load html for running trials, fill in appropriate text
    var that = this;
    $("body").load(this.htmlpath, function() {
        that.showEvidence();
    });
};


Experiment.prototype.showEvidence = function() {
    console.log("Showing evidence for trial: ", this.trial_index + 1);
    // Process trial object for this evidence trial
    var trialObj = this.trial_array[this.trial_index];
    var outcomeText = "";
    var outcomeImg = "";
    if (trialObj.outcome == 1) {
        outcomeText = this.fish_caught_msg;
        outcomeImg = this.fish_img_path;
    } else if (trialObj.outcome == 0) {
        outcomeText = this.no_fish_msg;
        outcomeImg = this.no_fish_img_path;
    }
    var shapeInfo = trialObj.evidence;
    var evidenceShape = new Lure(shapeInfo.top_shape, shapeInfo.bottom_shape,
        shapeInfo.top_color, shapeInfo.bottom_color, shapeInfo.top_texture, shapeInfo.bottom_texture);

    // Write to data object
    this.data["trial_data"].push({"trial_index": this.trial_index + 1});
    this.data["trial_data"][this.trial_index]["evidence_shape"] = trialObj.evidence;
    this.data["trial_data"][this.trial_index]["evidence_outcome"] = trialObj.outcome;

    // Update html to display this evidence trial
    var that = this;
    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#exp-container").load(this.evidence_htmlpath, function() {
        $("#evidence-outcome").text(outcomeText);
        $("#evidence-outcome-img-container").html("<img class='evidence-outcome-img' src='" + outcomeImg + "' />");
        evidenceShape.drawLure(canvasId = "evidence-shape-canvas", sizeConfig = "evidence"); // TODO store this ID somewhere sensible
        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            // TODO process time to click here (did they read this page?) and add data to experiment trial object
            that.showEvidenceResponse();
        });
    });
};


Experiment.prototype.showEvidenceResponse = function() {
    console.log("Collecting evidence response for trial: ", this.trial_index + 1);
    // Process trial object for this evidence response trial
    var trialObj = this.trial_array[this.trial_index];
    var responseBanner = "";
    var evidenceOutcomeImg = "";
    if (trialObj.outcome == 1) {
        evidenceOutcomeImg = this.fish_img_path_small;
        if (this.control) {
            responseBanner = this.fish_describe_msg;
        } else {
            responseBanner = this.fish_explain_msg;
        }
    } else if (trialObj.outcome == 0) {
        evidenceOutcomeImg = this.no_fish_img_path_small;
        if (this.control) {
            responseBanner = this.no_fish_describe_msg;
        } else {
            responseBanner = this.no_fish_explain_msg;
        }
    }

    var shapeInfo = trialObj.evidence;
    var evidenceShape = new Lure(shapeInfo.top_shape, shapeInfo.bottom_shape,
        shapeInfo.top_color, shapeInfo.bottom_color, shapeInfo.top_texture, shapeInfo.bottom_texture);

    // Update html for this response trial
    var that = this;
    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#exp-container").load(this.evidence_resp_htmlpath, function() {
        $("#evidence-response-banner").text(responseBanner);
        $("#evidence-response-banner").css("font-style", "Italic");
        evidenceShape.drawLure(canvasId = "obs-item-canvas-" + (that.trial_index + 1), sizeConfig = "observations"); // TODO store this ID somewhere sensible
        $("#obs-outcome-" + (that.trial_index + 1)).html("<img class='obs-outcome-img' src='" + evidenceOutcomeImg + "' />");
        if (trialObj.outcome == 1) {
            $("#obs-item-" + (that.trial_index + 1)).css("border", "5px solid black"); // draw bold box around fish catches
            $("#obs-item-" + (that.trial_index + 1)).css("margin", "5px"); // decrease margin to keep everything lined up
        }

        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            // TODO process whether they wrote anything here (prevent from clicking if they didn't write anything) and add what they wrote to experiment trial object
            that.showPrediction();
        });
    });
};


Experiment.prototype.showPrediction = function() {
    console.log("Collecting prediction for trial: ", this.trial_index + 1);
    // Process trial object for this prediction trial
    var trialObj = this.trial_array[this.trial_index];
    var shapeInfo = trialObj.prediction;
    var evidenceShape = new Lure(shapeInfo.top_shape, shapeInfo.bottom_shape, shapeInfo.top_color, shapeInfo.bottom_color);

    // Write to data object
    this.data["trial_data"][this.trial_index]["prediction_shape"] = trialObj.prediction;
    this.data["trial_data"][this.trial_index]["prediction_outcome"] = trialObj.prediction_outcome;

    // Update html for this prediction trial
    var that = this;
    $("#exp-container").empty(); // TODO consider making a separate function to clear stuff out, we call this a lot...
    $("#next-exp").hide();
    $("#exp-container").load(this.prediction_htmlpath, function() {
        that.trial_index += 1;
        evidenceShape.drawLure(canvasId = "prediction-shape-canvas", sizeConfig = "prediction"); // TODO store this ID somewhere sensible

        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            // TODO process whether they clicked everything here (prevent from clicking if they didn't click stuff) and add their data to experiment trial object
            if (that.trial_index >= that.trial_array.length) {
                console.log("Completed all trials.");
                that.showRuleGeneration();
            } else {
                that.showEvidence();
            }
        });
    });

};


Experiment.prototype.showRuleGeneration = function() {
    console.log("Collecting rule generation.");

    // Update html for rule generation
    var that = this;
    $("#obs-container").hide(); // TODO make separate function to clear out full trial stuff (observations etc.)
    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#exp-container").load(this.generate_htmlpath, function() {
        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            // TODO process whether they wrote anything here (prevent from clicking if they didn't write anything) and add what they wrote to experiment object
            that.showJudgmentTask();
        });
    });
};


Experiment.prototype.showJudgmentTask = function() {
    console.log("Collecting rule judgments.");
    // Update html for rule judgment task
    var that = this;
    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#exp-container").load(this.judgment_htmlpath, function() {
        for (jIndex = 1; jIndex <= that.judgment_array.length; jIndex++) {
            var jItem = that.judgment_array[jIndex - 1];
            var shapeInfo = jItem.probe;
            var jShape = new Lure(shapeInfo.top_shape, shapeInfo.bottom_shape, shapeInfo.top_color, shapeInfo.bottom_color);
            jShape.drawLure(canvasId = "generate-item-canvas-" + jIndex, sizeConfig = "generate"); // TODO store this ID somewhere sensible
        }

        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            // TODO process whether they clicked anything here (prevent from clicking next if they didn't) and add what they selected to experiment object
            that.showEvaluationTask();
        });
    });
};


Experiment.prototype.showEvaluationTask = function() {
    console.log("Showing rule evaluation for rule: ", this.eval_index + 1);
    var ruleEval = this.evalArray[this.eval_index];

    // Update html for evaluation task
    var that = this;
    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#exp-container").load(this.eval_htmlpath, function() {
        that.eval_index += 1;
        $("#obs-container").show(); // show observed evidence during evaluation task
        $("#eval-rule").text(ruleEval.rule_text);

        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            // TODO process whether they clicked anything here (prevent from clicking next if they didn't) and add what they selected to experiment object
            if (that.eval_index >= that.evalArray.length) {
                console.log("Completed all evaluations.");
                that.showMemoryTask();
            } else {
                that.showEvaluationTask();
            }
        });
    });
};


Experiment.prototype.showMemoryTask = function() {
    console.log("Showing memory task.");
    // Update html for memory task
    var that = this;
    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#obs-container").hide(); // TODO make separate function to clear out full trial stuff (observations etc.)
    $("#exp-container").load(this.memory_htmlpath, function() {
        for (memIndex = 1; memIndex <= that.memory_array.length; memIndex++) {
            var memItem = that.memory_array[memIndex - 1];
            var shapeInfo = memItem.probe;
            var memShape = new Lure(shapeInfo.top_shape, shapeInfo.bottom_shape, shapeInfo.top_color, shapeInfo.bottom_color);
            memShape.drawLure(canvasId = "memory-item-canvas-" + memIndex, sizeConfig = "memory"); // TODO store this ID somewhere sensible
        }

        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            // TODO process whether they clicked anything here (prevent from clicking next if they didn't) and add what they selected to experiment object
            that.endExperiment();
        });
    });
};


Experiment.prototype.endExperiment = function() {
    console.log("End of experiment!");

    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#exp-container").html("<h1>" + this.end_game_msg + "</h1>");

    // TODO write results to json!
};
