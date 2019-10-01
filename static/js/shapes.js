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
    console.log("Drawing lure: ", this);
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
    if (shape_str == "triangle") shapeObj = new Triangle(position, color);
    if (shape_str == "diamond") shapeObj = new Diamond(position, color);
    if (shape_str == "teardrop") shapeObj = new Teardrop(position, color);

    return shapeObj;
};



Circle = function(position, color) {
    if (position == "top") {
        this.x = 75;
        this.y = 75;
        this.radius = 25;
    } else if (position == "bottom") {
        this.x = 75;
        this.y = 110;
        this.radius = 10;
    }
    this.color = color;
};

Circle.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.arc(this.x, this.y, this.radius, 0, Math.PI * 2);
    ctx.fillStyle = this.color;
    ctx.fill();
};


Triangle = function(position, color) {
    if (position == "top") {
        this.top_x = 50;
        this.top_y = 50;
        this.base = 50;
        this.height = 50;
    } else if (position == "bottom") {
        this.top_x = 62.5;
        this.top_y = 100;
        this.base = 25;
        this.height = 25;
    }
    this.color = color;
};

Triangle.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.moveTo(this.top_x, this.top_y);
    ctx.lineTo(this.top_x + this.base, this.top_y);
    ctx.lineTo(this.top_x + (this.base / 2), this.top_y + this.height);
    ctx.closePath();
    ctx.fillStyle = this.color;
    ctx.fill();
};


Diamond = function(position, color) {
    if (position == "top") {
        this.top_x = 75;
        this.top_y = 50;
        this.width = 50;
        this.height = 50;
    } else if (position == "bottom") {
        this.top_x = 75;
        this.top_y = 100;
        this.width = 25;
        this.height = 25;
    }
    this.color = color;
};

Diamond.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.moveTo(this.top_x, this.top_y);
    ctx.lineTo(this.top_x - (this.width / 2), this.top_y + (this.height / 2));
    ctx.lineTo(this.top_x, this.top_y + this.height);
    ctx.lineTo(this.top_x + (this.width / 2), this.top_y + (this.height / 2));
    ctx.closePath();
    ctx.fillStyle = this.color;
    ctx.fill();
};


Teardrop = function(position, color) {
    if (position == "top") {
        this.top_x = 50;
        this.top_y = 50;
        this.width = 50;
    } else if (position == "bottom") {
        this.top_x = 62.5;
        this.top_y = 100;
        this.width = 25;
    }
    this.color = color;
};

Teardrop.prototype.draw = function(ctx) {
    // TODO replace this with straight lines down to < half circle arcs
    ctx.beginPath();
    ctx.arc(this.top_x, this.top_y, this.width / 2, 0, Math.PI / 2);
    ctx.arc(this.top_x + (this.width / 2), this.top_y + (this.width / 2), this.width / 2, Math.PI, 0, true);
    ctx.arc(this.top_x + this.width, this.top_y, this.width / 2, Math.PI / 2, Math.PI);
    ctx.fillStyle = this.color;
    ctx.fill();
};
