const elm = require("./elm").Elm.Main;
const config = {
    maxStates: 50,
    numberOfAnts: 10,
    gridWidth: 100,
    gridHeight: 100,
    cellwidth: 5
};

const element = document.createElement("canvas");
element.width = config.gridWidth * config.cellwidth;
element.height = config.gridHeight * config.cellwidth;

document.querySelector("#canvas").appendChild(element);

const context = element.getContext("2d");

const app = elm.init(
    {
        node: document.querySelector("#controls"),
        flags: config
    }
);


app.ports.draw.subscribe(
    drawables =>
        drawables.forEach(
            ([{ x, y }, color]) => {
                const cellwidth = config.cellwidth;
                context.fillStyle = color;
                context.fillRect(x * cellwidth, y * cellwidth, cellwidth, cellwidth);
            }
        )
);

