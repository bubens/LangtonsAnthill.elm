const elm = require("./elm").Elm.Main;

const config = {
    maxStates: 255,
    numberOfAnts: 10,
    gridWidth: 50,
    gridHeight: 50,
    cellwidth: 10
};

const app1 = elm.init(
    {
        node: document.querySelector("body"),
        flags: config
    });

/*
app.ports.inbox.send(JSON.stringify(obj));
app.ports.outbox.subscribe(
    data => console.log(data)
);
*/

