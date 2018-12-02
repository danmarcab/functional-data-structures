customElements.define('dot-render', class extends HTMLElement {
    set width(value) {
        if (this._width === value) return;
        this._width = value;
        this.render();
    }

    set height(value) {
        if (this._height === value) return;
        this._height = value;
        this.render();
    }

    set content(value) {
        if (this._content === value) return;
        this._content = value;
        this.render();
    }

    render() {
        var container = this;
        viz.renderSVGElement(container._content)
            .then(function (element) {
                var diagramSize =
                    {
                        width: element.getAttribute("width").replace("pt", "") * 1.33,
                        height: element.getAttribute("height").replace("pt", "") * 1.33
                    };

                widthRatio = container._width / diagramSize.width;
                heightRatio = container._height / diagramSize.height;

                if (widthRatio < heightRatio && widthRatio < 1) {
                    diagramSize.width *= widthRatio;
                    diagramSize.height *= widthRatio;
                }
                else if (heightRatio < widthRatio && heightRatio < 1) {
                    diagramSize.width *= heightRatio;
                    diagramSize.height *= heightRatio;
                }

                container.innerHTML = '';

                element.setAttribute("width", diagramSize.width);
                element.setAttribute("height", diagramSize.height);
                container.appendChild(element);
            });

    }
});
