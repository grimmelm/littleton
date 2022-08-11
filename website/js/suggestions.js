
/*** Suggestions interface ***/
export function showSuggestions(data) {
    let result = $.parseJSON(data),
        people = result.people,
        events = result.events;

    let person1_dropdown = $('#person1-list');
    let person2_dropdown = $('#person2-list');
    person1_dropdown.empty();
    person2_dropdown.empty();

    $.each(people, function (val, text) {
        let li = $('<li>').addClass("dropdown-item").val(val).text(text);
        li.click(function(event) {
            let target = $( event.target );
            target.parent().prev().text(text);
        });
        li.appendTo([person1_dropdown, person2_dropdown]);
    });

    let event_list = $('#event-list');
    event_list.empty();

    $.each(events, function (val, event) {
        let text = event.event;
        let li = $('<li>').addClass("dropdown-item").val(val).text(text);
        li.click(function(evt) {
            let target = $( evt.target );
            let event = events.find(element => element.event === target.html());
            if (event !== undefined && event.count > 1) {
                $("#Person2").show();
            } else {
                $("#Person2").hide();
            }
            target.parent().prev().text(text);
            showAddSuggestion(event);

        });
        event_list.append(li);
    });

    $("#add-suggestion").attr("hidden", true);
    if ($("#suggest").hasClass("active")) {
        $("#suggest").removeClass("active");
    }
}

export function eventlist_handler(events) {
    let selected = $("#Event option:selected");
    let ev = events.find(element => element.event === selected.html());
    if (ev !== undefined && ev.count > 1) {
        $("#Person2").show();
    }
    showAddSuggestion(event);
}

function fill_text(event) {
    let new_text = "";
    new_text += $("#Person1 option:selected").html() + " ";
    new_text += $("#Event option:selected").html();
    if ($("#Person2").is(":visible")) {
        new_text += " " + $("#Person2 option:selected").html();
    }
    $('#entry_text').val($('#entry_text').val().trim() + "\n" + new_text + ".");
}

export function suggest() {
    $("#Person1").show();
    $("#event").show();
}

export function showAddSuggestion(event) {
    let empty_string = " -- select -- ";
    if ($("#Person1 option:selected").html() === empty_string) return;
    if ($("#Event option:selected").html() === empty_string) return;
    if ($("#Person2").is(":visible") &&
        $("#Person2 option:selected").html() === empty_string) return;
    $("#add-suggestion").removeAttr('hidden');
}


export function addSuggestion(event) {
    let stmt = "";
    stmt += $('#Person1').html() + ' ' + $('#event').html() + ' ';
    if ($("#Person2").is(":visible")) {
        stmt += $('#Person2').html();
    }
    let new_text = $('#entry_text').val() + stmt + ".\n";
    $('#entry_text').val(new_text);
}

