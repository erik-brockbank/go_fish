/*
 * Experiment constants library
 */


/*
 * STARTUP CONSTANTS
 */
const HTML_LOOKUP = { // lookup table with human-understandable html file keys and the path to those files as vals
    "consent": "/static/html/consent.html",
    "instructions": "/static/html/inst.html",
    "experiment": "/static/html/exp.html",
    "evidence": "/static/html/evidence.html",
    "evidence_resp": "/static/html/evidence_response.html",
    "prediction": "/static/html/prediction.html",
    "generate": "/static/html/generate_input.html",
    "judgment": "/static/html/generate_task.html",
    "evaluation": "/static/html/evaluate.html",
    "memory": "/static/html/memory.html"
};


/*
 * INSTRUCTION CONSTANTS
 */

/*
 * INSTRUCTION_ARRAY is a list with identical dictionary elements used to display instructions.
 * Each element contains:
 *  "top_text": text to display in top instruction text bucket
 *  "canvas_img": path to image to display in center instruction canvas
 *  "bottom_text": text to display in bottom instruction text bucket
 */
const INSTRUCTION_ARRAY = [
    {
        top_text: "In this experiment, imagine you have a friend who loves fishing in a nearby lake. " +
                    "Your friend is trying to figure out what kinds of fishing lures are best for catching the fish in the lake. ",
        canvas_img: "img/inst-lure.png",
        bottom_text: "A fishing lure is something you put on the end of a fish hook in order to attract fish underwater. "
    },
    {
        top_text: "There’s only one kind of fish in the lake your friend fishes at. ",
        canvas_img: "img/inst-fish.png",
        bottom_text: "To keep things simple, assume that the fish in the lake always bite certain lure combinations and never bite others. "
    },
    {
        top_text: "You’re going to see a series of fishing lure combinations that your friend uses to try and catch fish. Here's an example: ",
        canvas_img: "img/inst-lure_combo.png",
        bottom_text: ""
    },
    {
        top_text: "Some lure combinations catch fish, and others don’t: ",
        canvas_img: "img/inst-lure_catch.png",
        bottom_text: "These are just examples. In the task today, you won’t see lures like this. "
    },
    {
        top_text: "The lure combinations you’ll see in today’s task all use the colors and shapes below: ",
        canvas_img: "img/inst-lure_shapes.png",
        bottom_text: ""
    },
    {
        top_text: "Your job is to figure out why your friend catches fish with some lure combinations and not with others. " +
            "Throughout the experiment, you will be asked to predict whether different lure combinations are likely to catch fish.",
        canvas_img: "",
        bottom_text: ""
    },
    {
        top_text: "Let's get started!",
        canvas_img: "",
        bottom_text: ""
    }
];


/*
 * EXPERIMENT CONSTANTS
 */
const OBSERVATIONS = "Observed lure combinations";
const OUTCOME_POSITIVE = "With these lures, she caught a fish!";
const OUTCOME_NEGATIVE = "With these lures, she didn't catch any fish.";
const CONTROL_RESPONSE_POS = "In the space below, describe this lure combination that your friend caught a fish with.";
const CONTROL_RESPONSE_NEG = "In the space below, describe this lure combination that your friend didn’t catch a fish with.";
const EXPLAIN_RESPONSE_POS = "In the space below, explain why your friend might have caught a fish with this lure combination.";
const EXPLAIN_RESPONSE_NEG = "In the space below, explain why your friend might not have caught any fish with this lure combination.";

/*
 * TRIAL_ARRAY is a list of dictionary objects containing information to be filled in when
 * displaying each trial during the experiment.
 * Each element contains:
 *  evidence: a dictionary object dictating what sort of lure combination is shown as evidence in this trial
 *  outcome: (bool) variable indicating whether this lure combination caught a fish
 *  prediction: a dictionary object of the same sort as `evidence` indicating what sort of lure combination
 *      should be used for the prediction on this trial
 */
const TRIAL_ARRAY = [
    {
        evidence: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        outcome: 1,
        prediction: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        }
    },
    {
        evidence: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        outcome: 0,
        prediction: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        }
    },
    {
        evidence: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        outcome: 0,
        prediction: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        }
    },
    {
        evidence: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        outcome: 1,
        prediction: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        }
    },
    {
        evidence: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        outcome: 1,
        prediction: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        }
    },
    {
        evidence: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        outcome: 0,
        prediction: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        }
    },
    {
        evidence: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        outcome: 1,
        prediction: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        }
    },
    {
        evidence: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        outcome: 0,
        prediction: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        }
    }
];

/*
 * EVAL_ARRAY is a list of dictionary objects containing the rules to be evaluated sequentially
 * during the evaluation task of the experiment.
 * Each element contains:
 *  rule_text: the string containing the rule to be evaluated
 *  is_target: a (bool) variable indicating whether this is the rule we are most interested in
 */
const EVAL_ARRAY = [
    {
        rule_text: "If a lure combination has a yellow shape, it will catch fish.",
        is_target: false // TODO consider including how much evidence each rule is consistent with
    },
    {
        rule_text: "If a lure combination contains a diamond, it will catch fish.",
        is_target: false
    },
    {
        rule_text: "There is no pattern to which lure combinations catch fish: the results are " +
            "random, but there are approximately equal numbers that catch fish and don’t.",
        is_target: false
    },
    {
        rule_text: "The lures that catch fish have a pointy shape on bottom.",
        is_target: true
    },
    {
        rule_text: "A lure combination will catch fish only if it has no blue shapes.",
        is_target: false
    }
];


/*
 * MEMORY_ARRAY is a list of dictionary objects containing the shapes to be used in the memory probe,
 * as well as whether they were in fact part of the experiment.
 */
const MEMORY_ARRAY = [
    {
        probe: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        in_expt: 1
    },
    {
        probe: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        in_expt: 1
    },
    {
        probe: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        in_expt: 1
    },
    {
        probe: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        in_expt: 1
    },
    {
        probe: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        in_expt: 1
    },
    {
        probe: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        in_expt: 1
    },
    {
        probe: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        in_expt: 1
    },
    {
        probe: {
            top_shape: "",
            top_color: "",
            bottom_shape: "",
            bottom_color: ""
        },
        in_expt: 1
    }
];
