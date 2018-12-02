// graphviz js
viz = require("./viz.js");
liteRender = require("./lite.render.js");
window.viz = new viz(liteRender);

// custom element to render .dot
customElements = require("./custom-elements.min.js");
dotRender = require("./dot-render.js")

// Elm app
window.app = require("../src/Main.elm").Elm.Main;
