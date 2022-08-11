import * as ui from './ui.js';

import rr from './railroad.js';

function visualizeResult(points, text) {
    let texts = [];
    let images = [];
    let commands = text.trim().split("\n");
    $.each(points, function (index, point) {
        let term = JSON.stringify(point.step, null, 2);
        let svg = rr.Diagram(ui.makeSVG(point.step)).toSVG();
        let text = $("<div>").addClass("point")
            .append($("<code>").append("After statement:\t" + point.statement))
            .append($("<code>").append("Remaining term:\t" + term));
        texts.push(text);
        images.push({ "event": commands[index], "svg": svg });
    });
    return [texts, images];
}

function renderResults(results, text) {
    let viz = visualizeResult(results, text);
    let texts = viz[0], graphics = viz[1];
    let renders = [];

    $.each(graphics, function (index, graphic) {
        let image = $("<div>").addClass("card-body").append(graphic.svg);
        let title = $("<h3>").addClass("event").append(graphic.event);
        let header = $("<div>").addClass("card-header").append(title);
        let card = $("<div>").addClass("card bg-success")
            .append(header).append(image);
        renders.push(card);
    });

    return renders;
}
function getButton(target) {
    if (target.is("button")) {
        return target;
    } else {
        return target.parent("button");
    }
}

function hideExample(event) {
    let target = $(event.target);
    let button = getButton(target);
    let container = button.parent(".container-tut-eg");

    container.find(".tut-results").remove();
    container.find("hr").remove();
    container.find(".btn-hide").attr("hidden", "");
}

function runExample(event) {
    let target = $(event.target);
    let button = getButton(target);
    let container = button.parent(".container-tut-eg");
    let id = container.attr("id");
    let text = container.find(".code-tut-eg").text();

    // Remove the previous results, if they exist
    let resultId = id + "-result";
    let prev = container.find("#" + resultId);
    if (prev) {
        container.find("#" + resultId).remove();
        container.find("hr").remove();
    }

    // Call littleton torun the example
    let response = $.parseJSON(littleton.interpret(text));

    if (response.success) {
        // Display the resulting interest graphs
        let renders = renderResults(response.success, text);
        let result = $("<div>").attr("id", resultId)
            .addClass("list-group")
            .addClass("tut-results");
        $.each(renders, function(index, render) { result.append(render);});
        container.append($("<hr>"));
        container.append(result);
    } else {
        let err = $("<div>").attr("id", resultId)
            .addClass("tut-results")
            .append(response.error);
        container.append("<hr>");
        container.append(err);
    }
    container.find(".btn-hide").removeAttr("hidden");
}

function editExample(event) {
    let target = $(event.target);
    let button = getButton(target);
    let container = button.parent(".container-tut-eg");
    let text = container.find(".code-tut-eg");

    let editable = text
        .attr("contenteditable", "true")
        .addClass("editable")
        .css("display", "inline-block");

    text.replaceWith(editable);
}


export function attachHandlers() {
    $(".btn-run").click(runExample);
    $(".btn-hide").click(hideExample);
    $(".btn-editable").click(editExample);
}
