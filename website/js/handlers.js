import * as ui from './ui.js';
import {getExampleLists} from './examples.js';
import {suggest, showAddSuggestion, addSuggestion, eventlist_handler} from './suggestions.js';
import {makeParameters} from './parameters.js';

/**
 * Go from the index page to the interpreter page
 */
function tryIt() {
    let conveyance = $("#entry_text").val();
    $(location).attr('href', '/interpreter.html?' + conveyance);
}

/**
 * Set up the try button handler on the index page
 */
export function tryHandler() {
    // Attach a handler to the form on the Index page
    $("#try_it").click(tryIt);
}

/**
 * Set up handlers for all the click-able buttons
 */
export function UIHandlers() {
    // Handlers for action menu items
    $("#action-interpret").click(ui.actionInterpret);
    $("#action-suggest").click(ui.actionSuggest);

    // Handlers for the Suggestion toolbar
    $("#suggest").click(suggest);
    $("#Person1").change(showAddSuggestion);
    $("#Person2").change(showAddSuggestion);
    $("#add-suggestion").click(addSuggestion);

    // Handlers for the Interpret toolbar
    $("#interpret").click(ui.interpret);
    $("#Event").change(eventlist_handler);

    // Grab the list of examples
    $(getExampleLists("examples/"));

    // Get and set up interface for parameters
    $(makeParameters());

    // Interpret the current conveyance
    ui.actionInterpret();
}
