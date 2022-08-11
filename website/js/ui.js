import rr from './railroad.js';

import {getExampleLists} from './examples.js';
import {suggest, showAddSuggestion, addSuggestion, eventlist_handler} from './suggestions.js';

/**
 * Take the given term in JSON format and generate an appropriate SVG
 * representation of it.
 *
 * @param {JSON} term The term created as a result of a Orlando program
 * @return {SVG} an SVG representation of the term visualized using the Railroad
 *               diagrams library
 */
export function makeSVG(term) {
    if (!term) { return rr.Skip(); }
    if (term == "bottom") { return rr.Skip(); }
    if (term.atom) { return rr.Terminal(term.atom); }
    else if (term.class) {
        return rr.Choice.apply(this,
                            [0,rr.NonTerminal("class gift to " + term.class.description)]
                            .concat(term.class.interests.map(makeSVG)));
    }
    else if (term.seq) {
        return rr.Sequence(makeSVG(term.seq[0]),
                        makeSVG(term.seq[1]));
    }
    else if (term.common) {
        return rr.Choice.apply(this,[0,rr.NonTerminal("tenants in common")].concat(term.common.map(makeSVG)));
    }
    else if (term.joint) {
        return rr.Choice.apply(this,[0,rr.NonTerminal("joint tenants")].concat(term.joint.map(makeSVG)));
    }
    else if (term.entireties) {
        return rr.Choice.apply(this,[0,rr.NonTerminal("tenants by the entireties")].concat(term.entireties.map(makeSVG)));
    }
    else if (term.while) {
        return rr.OneOrMore(rr.NonTerminal("while " + term.while.condition),
                         makeSVG(term.while.term));
    }
    else if (term.if) {
        return rr.Choice(0,
                         rr.Sequence(rr.NonTerminal("if " + term.if.condition), makeSVG(term.if.term1)),
                         rr.Sequence(rr.NonTerminal("otherwise"), makeSVG(term.if.term2))
                         );
    }
    else if (term.tail) {
        return rr.OneOrMore(rr.NonTerminal("while " + term.tail.issue + " has issue"),
                         makeSVG(term.tail.term));
    }
    else { return null; }
}


/**
 * Take a list of points (statements and the corresponding result), and convert
 * it to a list of textual terms and their SVG representation.
 *
 * @param {Point[]} a list of statements and the term after the statement is
 *                  executed
 * @return {(string, SVG)[]} a list of the string and SVG representation of the
 *                           term at each step
 */
function visualizeResult(points) {
    let texts = [];
    let images = [];
    let conveyance = $("#entry_text").val().trim().split("\n");
    $.each(points, function (index, point) {
        let term = JSON.stringify(point.step, null, 2);
        let svg = rr.Diagram(makeSVG(point.step)).toSVG();
        let text = $("<div>").addClass("point")
            .append($("<code>").append("After statement:\t" + point.statement))
            .append($("<code>").append("Remaining term:\t" + term));
        texts.push(text);
        images.push({ "event": conveyance[index], "svg": svg });
    });
    return [texts, images];
}


/**
 * Take a list of terms, create their visual representations in SVG and display
 * them on the page, along with the corresponding statement.
 *
 * @param {Point[]} a list of statements and the term after the statement is
 *                  executed
 * @return null
 */
function showResult(results) {
    let viz = visualizeResult(results);
    let texts = viz[0], graphics = viz[1];

    $("#error-list").empty();
    $("#errors").attr("hidden", true);

    $("#result").empty().append(texts);
    $("#images").empty();

    let images = $("<div>").attr("id", "images").addClass("list-group");
    $.each(graphics, function (index, graphic) {
        let image = $("<div>").addClass("card-body").append(graphic.svg);
        let title = $("<h3>").addClass("event").append(graphic.event);
        let header = $("<div>").addClass("card-header").append(title);
        let card = $("<div>").addClass("card bg-success")
            .append(header).append(image);
        images.append(card);
    });
    $("#images").replaceWith(images);
}

/**
 * Take the given error message and display it on the page.
 *
 * @param {string} err The error message to display
 */
function showError(err) {
    $("#result").empty();
    $("#result").hide();
    $("#graph").empty();
    $("#error-list").empty();

    let error = $("<li>")
        .addClass("error")
        .addClass("list-group-item").text(err);
    $("#error-list").append(error);
    $("#errors").removeAttr("hidden");
}

function showTrace() {
    if ($("#trace").hasClass("active")) {
        $("#result").hide();
        $("#errors").hide();
        $("#trace").removeClass("active").text("Show trace");
    } else {
        $("#result").show();
        $("#errors").show();
        $("#trace").addClass("active").text("Hide trace");
    }
}

/** UI to change the current action button. **/

/**
 * Call the Littleton interpreter on the current Orlando program in the text box
 *
 */
export function interpret() {
    // Get the conveyance from the text entry form
    // TRACE_INDEX = 0;
    let conveyance = $("#entry_text").val();
    let data = littleton.interpret(conveyance);
    let result = $.parseJSON(data);

    if (result.success) {
        showResult(result.success);
    } else if (result.error) {
        showError(result.error);
    }
    //stepEnd(event);
}

/**
 * Set up the interpret bar
 */
export function actionInterpret() {
    let action_button = $('.current-action');
    let action_toolbar = $('.current-toolbar');

    let interpret_button = $('#interpret').clone(true)
        .addClass('current-action').removeAttr("hidden");
    let interpret_bar = $('#interpret-bar').clone(true)
        .addClass('current-toolbar').removeAttr("hidden");

    action_button.replaceWith(interpret_button);
    action_toolbar.replaceWith(interpret_bar);
    interpret();
}

/**
 * Set up the suggestion toolbar
 */
export function actionSuggest() {
    let action_button = $('.current-action');
    let action_toolbar = $('.current-toolbar');

    let suggest_button = $('#suggest').clone(true)
        .addClass('current-action').removeAttr("hidden");
    let suggest_bar = $('#suggest-bar').clone(true)
        .addClass('current-toolbar').removeAttr("hidden");

    action_button.replaceWith(suggest_button);
    action_toolbar.replaceWith(suggest_bar);
    suggest();
}
