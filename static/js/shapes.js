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

Lure.prototype.drawLure = function(canvasId, sizeConfig) {
    this.drawShape(canvasId, sizeConfig, this.top_shape, this.top_color, position = "top");
    this.drawShape(canvasId, sizeConfig, this.bottom_shape, this.bottom_color, position = "bottom");
};

Lure.prototype.drawShape = function(canvasId, sizeConfig, shape_str, color_str, position) {
    var shapeObj = this.makeShapeObj(shape_str, color_str, position, sizeConfig);
    var canvas = document.getElementById(canvasId); // unclear why we can't use jquery here
    if (canvas.getContext && shapeObj) {
        var ctx = canvas.getContext("2d");
        shapeObj.draw(ctx)
    }
};

Lure.prototype.makeShapeObj = function(shape_str, color_str, position, sizeConfig) {
    // get color hex val for this shape
    var color = "";
    if (color_str in COLOR_LOOKUP) color = COLOR_LOOKUP[color_str];

    // get appropriate shape class for this shape
    var shapeObj;
    if (shape_str == "circle") shapeObj = new Circle(sizeConfig, position, color);
    if (shape_str == "triangle") shapeObj = new Triangle(sizeConfig, position, color);
    if (shape_str == "diamond") shapeObj = new Diamond(sizeConfig, position, color);
    if (shape_str == "teardrop") shapeObj = new Teardrop(sizeConfig, position, color);

    return shapeObj;
};



Circle = function(sizeConfig, position, color) {
    if (sizeConfig == "evidence" || sizeConfig == "prediction") {
        if (position == "top") {
            this.x = 75;
            this.y = 75;
            this.radius = 25;
        } else if (position == "bottom") {
            this.x = 75;
            this.y = 110;
            this.radius = 10;
        }
    }
    if (sizeConfig == "observations") {
        if (position == "top") {
            this.x = 40;
            this.y = 55;
            this.radius = 25;
        } else if (position == "bottom") {
            this.x = 40;
            this.y = 90;
            this.radius = 10;
        }
    }
    if (sizeConfig == "generate" || sizeConfig == "memory") {
        if (position == "top") {
            this.x = 30;
            this.y = 55;
            this.radius = 25;
        } else if (position == "bottom") {
            this.x = 30;
            this.y = 90;
            this.radius = 10;
        }
    }
    this.color = color;
};

Circle.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.arc(this.x, this.y, this.radius, 0, Math.PI * 2);
    ctx.fillStyle = this.color;
    ctx.fill();
};


Triangle = function(sizeConfig, position, color) {
    if (sizeConfig == "evidence" || sizeConfig == "prediction") {
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
    }
    if (sizeConfig == "observations") {
        if (position == "top") {
            this.top_x = 15;
            this.top_y = 30;
            this.base = 50;
            this.height = 50;
        } else if (position == "bottom") {
            this.top_x = 27.5;
            this.top_y = 80;
            this.base = 25;
            this.height = 25;
        }
    }
    if (sizeConfig == "generate" || sizeConfig == "memory") {
        if (position == "top") {
            this.top_x = 5;
            this.top_y = 30;
            this.base = 50;
            this.height = 50;
        } else if (position == "bottom") {
            this.top_x = 17.5;
            this.top_y = 80;
            this.base = 25;
            this.height = 25;
        }
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


Diamond = function(sizeConfig, position, color) {
    if (sizeConfig == "evidence" || sizeConfig == "prediction") { // TODO is there a cleaner way to do this?
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
    }
    if (sizeConfig == "observations") { // TODO is there a cleaner way to do this?
        if (position == "top") {
            this.top_x = 40;
            this.top_y = 30;
            this.width = 50;
            this.height = 50;
        } else if (position == "bottom") {
            this.top_x = 40;
            this.top_y = 80;
            this.width = 25;
            this.height = 25;
        }
    }
    if (sizeConfig == "generate" || sizeConfig == "memory") { // TODO is there a cleaner way to do this?
        if (position == "top") {
            this.top_x = 30;
            this.top_y = 30;
            this.width = 50;
            this.height = 50;
        } else if (position == "bottom") {
            this.top_x = 30;
            this.top_y = 80;
            this.width = 25;
            this.height = 25;
        }
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


Teardrop = function(sizeConfig, position, color) {
    if (sizeConfig == "evidence" || sizeConfig == "prediction") {
        if (position == "top") {
            this.top_x = 50;
            this.top_y = 50;
            this.width = 50;
        } else if (position == "bottom") {
            this.top_x = 62.5;
            this.top_y = 112.5;
            this.width = 25;
        }
    }
    if (sizeConfig == "observations") {
        if (position == "top") {
            this.top_x = 15;
            this.top_y = 30;
            this.width = 50;
        } else if (position == "bottom") {
            this.top_x = 27.5;
            this.top_y = 92.5;
            this.width = 25;
        }
    }
    if (sizeConfig == "generate" || sizeConfig == "memory") {
        if (position == "top") {
            this.top_x = 5;
            this.top_y = 30;
            this.width = 50;
        } else if (position == "bottom") {
            this.top_x = 17.5;
            this.top_y = 92.5;
            this.width = 25;
        }
    }
    this.color = color;
};

Teardrop.prototype.draw = function(ctx) {
    ctx.beginPath();
    ctx.arc(this.top_x + (this.width / 2), this.top_y + (this.width / 2), this.width / 2, (5/4) * Math.PI, (7/4) * Math.PI, true);
    ctx.lineTo(this.top_x + (this.width / 2), this.top_y - (this.width / 2));
    ctx.closePath();
    ctx.fillStyle = this.color;
    ctx.fill();
};
