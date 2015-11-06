// ==UserScript==
// @name        add-remove
// @namespace   tk
// @include     https://tk-sc.intern.sigma-chemnitz.de/Employees/Details/*
// @include     https://tk-ssc.intern.sigma-chemnitz.de/Employees/Details/*
// @version     1
// @grant       none
// ==/UserScript==
var FIXUP_MARKER = "ensc-fixup";
var REMOVE_METHOD = 0;

var DEL_FIELDS = {
    "Effort"		: "0",
    "From"		: "00:00",
    "To"		: "00:00",
    "NotInvoiceable"	: "0",
    "InternalComments"	: "",
    "ExternalComments"	: "***",
    "Break"		: "0",
};

function _remove_data_form(doc, uri)
{
    var f = null;

    for (var i = 0; i < doc.forms.length; ++i) {
	var f = doc.forms[i];

	if (f.action.search(uri) >= 0 && f.method == "post")
	    break;
    }

    if (!f) {
	alert("expected formular not found!");
	return;
    }

    for (df in DEL_FIELDS)
	f.elements[df].value = DEL_FIELDS[df];

    // TODO: why does 'f.submit()' not work here?

    var data = new FormData();
    for (var i = 0; i < f.elements.length; ++i) {
	var e = f.elements[i];
	if (e.name)
	    data.append(e.name, e.value);
    }

    xhr = new XMLHttpRequest();
    xhr.open("POST", uri);
    xhr.setRequestHeader('Referer', doc.uri);

    xhr.addEventListener("error", function(ev) {
	alert("Error: " + ev);
    });

    xhr.onreadystatechange = function () {
        if (xhr.readyState == 4) {
	    if (xhr.status != 200) {
		alert("Failed to submit data: " + xhr.statusText);
		return;
	    }

	    // alert("form submitted");
	}
    };

    xhr.send(data);
}

function _remove_data_complex(key)
{
    var uri = "/Times/Edit/" + encodeURIComponent(key);
    var xhr = new XMLHttpRequest();

    xhr.open("GET", uri, true);
    xhr.onreadystatechange = function () {
        if (xhr.readyState == 4) {
	    if (xhr.status != 200) {
		alert("Failed to get form: " + xhr.statusText);
		return;
	    }

	    _remove_data_form(xhr.responseXML, uri);
        }
    };
    xhr.responseType = "document";
    xhr.send();
    return;
}

function _remove_data_simple(key)
{
    var xhr = new XMLHttpRequest();
    var data = new FormData();
    var now = new Date();

    for (f in DEL_FIELDS)
	data.append(f, DEL_FIELDS[f]);

    data.append('Date', "01." + (now.getMonth() + 1) + "." + now.getFullYear());

    xhr.addEventListener("error", function(ev) {
	alert("Error: " + ev);
    });

    xhr.open("POST", "/Times/Edit/" + encodeURIComponent(key));
    xhr.send(data);
}

function handle_colgroup(cols)
{
    var el = document.createElement("col");
    el.setAttribute("style", "width: 3em");

    cols.insertBefore(el, cols.firstElementChild);
}

function handle_thead(tr)
{
    tr.insertCell(0);
}

function handle_tbody(tr)
{
    var data_key = tr.getAttribute("data-key");
    var cell = tr.insertCell(0);

    var btn = document.createElement("button");
    btn.addEventListener("click", function() {
	if (confirm("Really?")) {
	    switch (REMOVE_METHOD) {
	    case 0:
		_remove_data_simple(data_key);
		break;
	    case 1:
		_remove_data_complex(data_key);
		break;
	    default:
		assert(false);
	    }
	}

	return true;
    });
    btn.innerHTML = "X";

    cell.appendChild(btn);
}

function fixup_element_single(el, fn)
{
    if (el.classList.contains(FIXUP_MARKER))
	return;

    el.classList.add(FIXUP_MARKER);
    fn(el);
}

function fixup_element(table, tagname, childtag, fn)
{
    el = table.getElementsByTagName(tagname);
    if (!el || el.length != 1)
	return;

    if (childtag == null) {
	fixup_element_single(el[0], fn);
	return;
    }

    children = el[0].getElementsByTagName(childtag);
    if (!children)
	return;

    for (var i = 0; i < children.length; ++i)
	fixup_element_single(children[i], fn);
}

function fixup_delete()
{
    table = document.getElementById('timeslist');
    if (!table)
	return;

    fixup_element(table, "colgroup", null, handle_colgroup);
    fixup_element(table, "thead", "tr", handle_thead);
    fixup_element(table, "tbody", "tr", handle_tbody);
}

fixup_delete();
