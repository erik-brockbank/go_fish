
Instructions = function(instructionSet) {
    this.instructionSet = instructionSet; // Array of instruction text elements to display
    this.instructionsComplete = false; // Bool to flip when all instructions have been shown
    this.instructionsIndex = 0; // Index to keep track of how many instructions have been processed
}

Instructions.prototype.run = function(experimentCallback) {
    console.log("starting instructions");
    this.callback = experimentCallback; // Function to call when instructions complete

    // Load html for displaying instructions
    var that = this;
    $("body").load("inst.html", function() {
        if(that.instructionsIndex > 0 || that.instructionSet.length < 1) {
            console.warn("Starting instructions in bad state");
        } else {
            that.populateInstruction()
        }
        $("#next-inst").click(function() {that.buttonNext();});
    });
}

/*
 * Function called by button click, used for moving through instruction flow
 */
Instructions.prototype.buttonNext = function() {
    console.log("Button click! Instruction index: ", this.instructionsIndex);
    // console.log(this);

    if (this.instructionsIndex >= this.instructionSet.length) {
        console.log("End of instructions");
        this.callback();
    } else {
        this.populateInstruction();
    }
}


/*
 * Function to populate instruction html elements with appropriate text/images
 * during each phase of instructions
*/
Instructions.prototype.populateInstruction = function() {
    // console.log(this.instructionSet);
    console.log("Loading instruction elem at index: ", this.instructionsIndex);
    instructionElem = this.instructionSet[this.instructionsIndex];
    // console.log(instructionElem);

    if (instructionElem.index == this.instructionsIndex) {
        // Remove any existing images in the canvas
        $(".instruction-img").remove();
        // Add top text
        $("#text-top").text(instructionElem.top_text);
        // Add bottom text
        $("#text-bottom").text(instructionElem.bottom_text);
        // Add and format image
        $("#canvas-mid").prepend("<img class='instruction-img' src='"+instructionElem.canvas_img+"' />");
        $(".instruction-img").width(400);
    } else {
        console.warn("Mismatched instruction elements and indices");
        // TODO when this is true, search for element with correct index, throw error if DNE
    }

    this.instructionsIndex++;
    return
}
