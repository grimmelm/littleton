/*** Examples ***/

// Populate the example box when an example is clicked
function populate() {
    let text = $(this).attr("data-content");
    $("#entry_text").empty();
    $("#entry_text").val(text);
}

function useThis() {
    let i = $ ("<i>").addClass("fas fa-file-upload");
    let msg = $("<span>")
        .addClass("badge badge-success badge-up")
        .append(i);
    $(this).prepend(msg);
}

function removeUseThis() {
    $( this ).find("span").last().remove();
}

export function getExampleLists(root) {
    let request = $.getJSON(root + "index.json");
    request.done(function (examples) {
        if (examples.length == 1) {
            makeExpandedExampleList(root, examples.pop());
            return;
        }

        $.map(examples, function (filename, i) {
            if (i==0) {
                makeExampleList(root, filename, true);
            } else {
                makeExampleList(root, filename);
            }
        });
    });
}

function makeExample(name, example, index) {
    let lines = example.program.split("\n");
    let paras = [];
    $.each(lines, function (i, line) {
        if (!line.includes("owns")) { paras.push($('<p>').text(line)); }
    });
    let li = $("<li>")
        .attr("id", name + "_" + index)
        .attr("data-toggle", "popover")
        .attr("data-content", example.program)
        .addClass("example")
        .addClass("list-group-item")
        .click(populate)
        .hover(useThis, removeUseThis)
        .append(paras);
    return li;
}

function makeExampleNamed(name) {
    return function(example, index) {
        let lines = example.program.split("\n");
        let paras = [];
        $.each(lines, function (i, line) {
            if (!line.includes("owns")) { paras.push($('<p>').text(line)); }
        });
        let li = $("<li>")
            .attr("id", name + "_" + index)
            .attr("data-toggle", "popover")
            .attr("data-content", example.program)
            .addClass("example")
            .addClass("list-group-item")
            .click(populate)
            .hover(useThis, removeUseThis)
            .append(paras);
        return li;
    };
}

function makeExpandedExampleList(root, file) {
    let name = file.split('.').slice(0, -1).join('.');
    let request = $.getJSON(root + file);

    return request.done(function (result) {
        let eg = result[1];

        // Make the header
        let title = $("<h3>")
            .text(eg.title);
        let header = $("<div>")
            .addClass("card-header clearfix")
            .append(title);

        // Make the dropdown list
        let list = $("<ul>")
            .addClass("example-list list-group")
            .attr("id", name + "-list");
        let body = $("<div>")
            .addClass("card-body")
            .append(list);

        // Assemble the card
        let exampleList = $("<div>")
            .addClass("card bg-primary")
            .append(header)
            .append(body);

        // Add examples and attach click handlers
        let items = eg.examples.map(makeExampleNamed(name)) ;
        $(list).empty().append(items);
        $("#examples").append(exampleList);
    });
}

function makeExampleList(root, file, show=false) {
    let name = file.replace(".json", "");
    let request = $.getJSON(root + file);

    return request.done(function (result) {
        let eg = result[1];

        // Make the header
        let button = $("<button>")
            .addClass("btn btn-block text-left btn-example dropdown-toggle")
            .attr("id", name + "-button")
            .attr("type", "button")
            .attr("data-toggle", "collapse")
            .attr("data-target", "#" + name)
            .attr("aria-expanded", "false")
            .text(eg.title);
        let title = $("<h3>")
            .append(button);
        let header = $("<div>")
            .addClass("card-header clearfix")
            .append(title);

        // Make the dropdown list
        let list = $("<ul>")
            .addClass("example-list list-group")
            .attr("id", name + "-list");
        let body = $("<div>")
            .addClass("card-body")
            .append(list);
        let collapse = $("<div>")
            .addClass("collapse")
            .attr("id", name)
            .append(body);

        // Assemble the item
        let exampleList = $("<div>")
            .addClass("card example-list")
            .append(header)
            .append(collapse);

        $("#examples").append(exampleList);

        // Add examples and attach click handlers
        let egs = eg.examples.map(makeExampleNamed(name));
        $(list).empty().append(egs);
        $(list).children().click(populate);

        if (show) {
            collapse.collapse('show');
        }
    });
}
