


Experiment = function(istest, control) {
    this.istest = istest; // `test` experiments simulate a real experiment but write results as TEST_{exptid}.json
    this.control = control; // `control` holds condition: TRUE if control, else FALSE

    this.observationsText = OBSERVATIONS;
}


Experiment.prototype.run = function() {
    inst = new Instructions(INSTRUCTION_ARRAY);
    inst.run(this.startTrials.bind(this));
}

Experiment.prototype.startTrials = function() {
    console.log("starting experiment trials");
    console.log(this);

    // Load html for running trials, fill in appropriate text
    that = this;
    $("body").load("exp.html", function() {
        $("#observations").text(that.observationsText);
    });
}
