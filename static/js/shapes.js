/*
 * Helper library for drawing lure combination shapes
 * Uses canvas and global shape information provided in constants.js
 */



/*
 * Lure class for drawing lure combinations made up of shape classes defined below (circle, triangle, etc.)
 */
Lure = function(top_shape, bottom_shape, top_color, bottom_color) {
    this.top_shape = top_shape;
    this.bottom_shape = bottom_shape;
    this.top_color = top_color;
    this.bottom_color = bottom_color
};

Lure.prototype.drawLure = function(canvasId) {
    this.drawShape(canvasId, this.top_shape, this.top_color, position = "top");
    this.drawShape(canvasId, this.bottom_shape, this.bottom_color, position = "bottom");
};

Lure.prototype.drawShape = function(canvasId, shape_str, color_str, position) {
    var shapeObj = this.makeShapeObj(shape_str, color_str, position);
    var canvas = document.getElementById(canvasId); // unclear why we can't use jquery here
    if (canvas.getContext && shapeObj) {
        var ctx = canvas.getContext("2d");

        shapeObj.draw(ctx)
    }
};

Lure.prototype.makeShapeObj = function(shape_str, color_str, position) {
    // get color hex val for this shape
    var color = "";
    if (color_str in COLOR_LOOKUP) color = COLOR_LOOKUP[color_str];

    // get appropriate shape class for this shape
    var shapeObj;
    if (shape_str == "circle") shapeObj = new Circle(position, color);

    return shapeObj;
};



Circle = function(position, color) {
    // TODO customize these params
    if (position == "top") {
        this.x = 75;
        this.y = 75;
        this.radius = 50;
        this.startAngle = 0;
        this.endAngle = Math.PI * 2;
    } else if (position == "bottom") {
        this.x = 75;
        this.y = 75;
        this.radius = 50;
        this.startAngle = 0;
        this.endAngle = Math.PI * 2;
    }
    this.color = color;
};

Circle.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.arc(this.x, this.y, this.radius, this.startAngle, this.endAngle);
    ctx.fillStyle = this.color;
    ctx.fill();
};
