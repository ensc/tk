// ==UserScript==
// @name        add-remove
// @namespace   tk
// @include     https://tk-sc.intern.sigma-chemnitz.de/Employees/Details/*
// @version     1
// @grant       none
// ==/UserScript==
var FIXUP_MARKER = "ensc-fixup";

function _remove_data(key)
{
    var xhr = new XMLHttpRequest();
    var data = new FormData();
    var now = new Date();
    var fields = {
	"Break"		: "0",
	"Date"		: "01." + (now.getMonth() + 1) + "." + now.getFullYear(),
	"Effort"	: "0",
	"From"		: "00:00",
	"To"		: "00:00",
	"NotInvoiceable"	: "0",
	"InternalComments"	: "",
	"ExternalComments"	: "***",
    }

    for (f in fields)
	data.append(f, fields[f]);

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
	if (confirm("Really?"))
	    _remove_data(data_key);

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
