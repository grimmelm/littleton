/*** Parameters ***/

export function makeParameters() {
    let ps = littleton.parameters(null);
    let params = $.parseJSON(ps);

    // Make the dropdown button
    let button = $("<button>")
        .addClass("btn btn-block text-left btn-param dropdown-toggle")
        .attr("id", "param-button")
        .attr("type", "button")
        .attr("data-toggle", "collapse")
        .attr("data-target", "#param")
        .attr("aria-expanded", "false")
        .text("Settings");
    // Make the header
    let title = $("<h3>")
        .append(button);
    let header = $("<div>")
        .addClass("card-header clearfix")
        .append(title);

    // Make the dropdown list
    let list = $("<form>")
        .attr("id", "param-list");
    let body = $("<div>")
        .addClass("card-body")
        .append(list);
    let collapse = $("<div>")
        .addClass("collapse")
        .attr("id", "param")
        .append(body);

    // Assemble the card
    let parameterList = $("<div>")
        .addClass("card bg-primary")
        .append(header)
        .append(collapse);

    let items = params.map(makeParameter(name)) ;
    $(list).empty().append(items);
    $("#parameters").append(parameterList);
}

function makeParameter(name) {
    return function(param, index) {

        let input = makeOptions(param);
        let right_col = $("<div>")
            .addClass("col-3")
            .append(input);

        let label = $("<label>")
            .addClass("form-check-label")
            .attr("for", param.abbrev)
            .text(param.name);
        let left_col = $("<div>")
            .addClass("col-9")
            .append(label);

        let item = $("<div>")
            .attr("id", param.abbrev + "-item" )
            .attr("data-toggle", "popover")
            .attr("data-content", param.name)
            .addClass("parameter")
            .addClass("form-row")
            .append(left_col)
            .append(right_col);
        return item;
    };
}

function makeOptions(param) {
    if (param.options[0] == "Boolean") {
        let toggle = $("<input>")
            .attr("id", param.abbrev)
            .attr("type", "checkbox")
            .change(checkParam);

        if (param.current[0] == "Yes") {
            toggle = toggle.attr("checked", "checked");
        }
        return toggle ;
    } else if (param.options[0] == "OneOf") {
        let choices = param.options[1];
        let selector = $("<select>")
            .addClass("form-control")
            .attr("id", param.abbrev)
            .change(selectParam);
        let options = choices.map(function(choice){
            let option = $("<option>")
                .attr("value", choice[0])
                .text(choice[1]);
            selector.append(option);
        });
        selector.val(param.current[1]).change();
        return selector;
    }
}

function checkParam(event) {
    event.preventDefault();
    let param = event.target.id;
    if ($(this).is(':checked')) {
        // console.log("Id ", param, " has value true");
        littleton.setParameter(param, "true");
    } else {
        // console.log("Id ", param, " has value false");
        littleton.setParameter(param, "false");
    }
    littleton.getParameter(param)
}

function selectParam(event) {
    event.preventDefault();
    let value = $(this).find(':selected').val();
    let param = event.target.id;
    // console.log("Id ", param, " has value ", value);
    littleton.setParameter(param, value)
}
