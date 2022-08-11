let TRACE_INDEX = 0;
let graphics = [];

export function stepForward(event) {
    event.preventDefault();
    if (graphics == []) return;
    TRACE_INDEX = Math.min(TRACE_INDEX + 1, graphics.length - 1);
    stepGraphToCard(graphics.slice(0, TRACE_INDEX + 1));

}

export function stepBackward(event) {
    event.preventDefault();
    if (graphics == []) return;
    TRACE_INDEX = Math.max(0, TRACE_INDEX - 1);
    stepGraphToCard(graphics.slice(0, TRACE_INDEX + 1));
}

export function stepStart(event) {
    event.preventDefault();
    TRACE_INDEX = 0;
    stepGraphToCard(graphics.slice(0, TRACE_INDEX + 1));
}

export function stepEnd(event) {
    // event.preventDefault();
    if (graphics == []) return;
    TRACE_INDEX = graphics.length - 1;
    stepGraphToCard(graphics.slice(0, TRACE_INDEX + 1));
}

export function stepGraphToCard(graphics) {
    let images = $("<div>").attr("id", "graph").addClass("list-group");
    $.each(graphics, function (index, graphic) {
        let image = $("<div>").addClass("card-body").append(graphic.svg);
        let title = $("<h3>").addClass("event").append(graphic.event);
        let header = $("<div>").addClass("card-header").append(title);
        let card = $("<div>").addClass("card bg-success")
            .append(header).append(image);
        images.append(card);
        console.log("Appending images to result");
    });
    $("#graph").replaceWith(images);
}

