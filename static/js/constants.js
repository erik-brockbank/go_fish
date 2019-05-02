
/*
 * INSTRUCTION CONSTANTS
 */



/*
 * INSTRUCTION_ARRAY is a list with identical dictionary elements that each contain:
 *  "index": index this element should be shown at in instructions
 *  "top_text": text to display in top instruction text bucket
 *  "canvas_img": path to image to display in center instruction canvas
 *  "bottom_text": text to display in bottom instruction text bucket
 */
const INSTRUCTION_ARRAY = [
    {
        index: 0,
        top_text: "In this experiment, imagine you have a friend who loves fishing in a nearby lake. " +
                    "Your friend is trying to figure out what kinds of fishing lures are best for catching the fish in the lake. ",
        canvas_img: "img/inst-lure.png",
        bottom_text: "A fishing lure is something you put on the end of a fish hook in order to attract fish underwater. "
    },
    {
        index: 1,
        top_text: "There’s only one kind of fish in the lake your friend fishes at. ",
        canvas_img: "img/inst-fish.png",
        bottom_text: "To keep things simple, assume that the fish in the lake always bite certain lure combinations and never bite others. "
    },
    {
        index: 2,
        top_text: "You’re going to see a series of fishing lure combinations that your friend uses to try and catch fish. Here's an example: ",
        canvas_img: "img/inst-lure_combo.png",
        bottom_text: ""
    },
    {
        index: 3,
        top_text: "Some lure combinations catch fish, and others don’t: ",
        canvas_img: "img/inst-lure_catch.png",
        bottom_text: "These are just examples. In the task today, you won’t see lures like this. "
    },
    {
        index: 4,
        top_text: "The lure combinations you’ll see in today’s task all use the colors and shapes below: ",
        canvas_img: "img/inst-lure_shapes.png",
        bottom_text: ""
    },
    {
        index: 5,
        top_text: "Your job is to figure out why your friend catches fish with some lure combinations and not with others. ",
        canvas_img: "",
        bottom_text: "Throughout the experiment, you will be asked to predict whether different lure combinations are likely to catch fish. "
    },
    {
        index: 6,
        top_text: "Let's get started!",
        canvas_img: "",
        bottom_text: ""
    }
];


/*
 * EXPERIMENT CONSTANTS
 */

const OBSERVATIONS = "Observed lure combinations";
