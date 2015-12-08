// ==UserScript==
// @name        xtra-detail
// @namespace   tk
// @version     1.0.1
// @include     https://tk-*c.intern.sigma-chemnitz.de/Employees/Details/*
// @include     https://tk-*.intern.sigma-chemnitz.de/Tasks/Details/*
// @include     https://tk-*.intern.sigma-chemnitz.de/Phases/Details/*
// @grant       none
// ==/UserScript==
var FIXUP_MARKER = "ensc-fixup";

var DATE_FMT_OPTS = { year: "numeric", month: "2-digit", day: "2-digit" ,
		      weekday: "short" };
var DATE_FMT = new Intl.DateTimeFormat('de-DE', DATE_FMT_OPTS);

function emit_event(inp, ev)
{
    var e = document.createEvent("HTMLEvents");
    e.initEvent(ev, false, true);
    inp.dispatchEvent(e);
}

function select_time_input(inp, tm)
{
    var col = inp.getAttribute("data-column");
    if (col != "0")
	return;

    var txt = "" + (tm.getMonth() + 1) + "." + tm.getFullYear();

    inp.value = txt;
    emit_event(inp, "keyup");
}

function select_time(tm)
{
    var table = document.getElementById('timeslist');
    if (!table)
	return;

    var thead = table.getElementsByTagName("thead");
    if (!thead || thead.length != 1)
	return;
    thead = thead[0];

    var tr = thead.getElementsByClassName("tablesorter-filter-row");
    if (!tr || tr.length != 1)
	return;

    tr = tr[0];

    var inputs = tr.getElementsByTagName("input");
    if (!inputs)
	return;

    for (var i = 0; i < inputs.length; ++i)
	select_time_input(inputs[i], tm);
}

function fixup_h1(h1, info, summary)
{
    var btn_hide = document.createElement("button");
    var btn_summary = document.createElement("button");
    var btn_srefresh = document.createElement("button");

    btn_hide.addEventListener("click", function() {
	info.hidden = !info.hidden;
    });

    btn_hide.innerHTML = "H";
    h1.appendChild(btn_hide);

    btn_summary.addEventListener("click", function() {
	summary.__el.hidden = !summary.__el.hidden;
	btn_srefresh.disabled = summary.__el.hidden;
    });
    btn_summary.innerHTML = "S";
    h1.appendChild(btn_summary);


    btn_srefresh = document.createElement("button");
    btn_srefresh.addEventListener("click", function() {
	summary.refresh();
    });
    btn_srefresh.innerHTML = "R";
    h1.appendChild(btn_srefresh);


    var btn_month_prev = document.createElement("button");
    btn_month_prev.addEventListener("click", function() {
	var now = new Date();
	select_time(new Date(now.getFullYear(), now.getMonth() - 1, 1, 0, 0, 0));
    });
    btn_month_prev.innerHTML = "-1"
    h1.appendChild(btn_month_prev);

    var btn_month_cur = document.createElement("button");
    btn_month_cur.addEventListener("click", function() {
	var now = new Date();
	select_time(new Date(now.getFullYear(), now.getMonth(), 1, 0, 0, 0));
    });
    btn_month_cur.innerHTML = "+0"
    h1.appendChild(btn_month_cur);
}

function fixup_element_single(el, fn)
{
    if (el.classList.contains(FIXUP_MARKER))
	return;

    el.classList.add(FIXUP_MARKER);
    fn(el);
}

var DateEntry = function(row) {
    this.parse_date = function(content) {
	return new Date(content.substr(6,4),
			content.substr(3,2)-1,
			content.substr(0,2),
			0, 0, 0);
    }

    this.parse_duration = function(content) {
	var unit_pos = content.indexOf(" ");
	var comma_pos = content.indexOf(",");

	if (comma_pos < 0 || unit_pos < 0 || unit_pos < comma_pos)
	    // todo: throw exception
	    return "n/a";

	if (content.substr(unit_pos+1) != "Std.")
	    // todo: throw exception
	    return "n/a";

	var hour = content.substr(0,comma_pos);
	var min  = content.substr(comma_pos+1,unit_pos - comma_pos);

	return Number(hour + '.' + min) * 60;
    };

    var tds = row.getElementsByTagName("td");

    this.date     = this.parse_date(tds[0].textContent);
    this.date.get_week = function() {
	var tmp = new Date(this.getFullYear(), 0, 1);

	return Math.floor((((this - tmp) / 86400000) +
			   ((tmp.getDay() + 1) % 7)) / 7);
    }
    this.date.getDayMo = function() {
	return (this.getDay() + 6) % 7;
    };

    this.date._addDay = function(delta) {
	var tmp = new Date(this.getFullYear(), this.getMonth(),
			   this.getDate() + delta, 0, 0, 0);

	return tmp;
    }

    this.date.isSameWeek = function(b) {
	var tmp_a = this._addDay(-this.getDayMo());
	var tmp_b = b._addDay(-b.getDayMo());

	return tmp_a - tmp_b == 0;
    }

    this.duration = this.parse_duration(tds[2].textContent);
};

var Counter = function() {
    this.v = 0;

    this.add = function(v) {
	this.v += v;
    }
};

function hline(cols) {
    var tr = document.createElement("tr");
    var td = document.createElement("td");

    td.colSpan = cols;
    td.innerHTML = "-";
    tr.appendChild(td);

    return tr;
};

function _create_line(td0_content, cnt)
{
    var tr  = document.createElement("tr");
    var td0 = document.createElement("td");
    var td1 = document.createElement("td");

    td0.textContent = td0_content;
    td0.align = "right";

    td1.textContent = ((cnt/60).toFixed(0) + ":" + ('0' + cnt%60).slice(-2));
    td1.align = "right";
    td1.style['padding-left'] = "2em";

    tr.appendChild(td0);
    tr.appendChild(td1);

    return tr;
}

function total_line(total) {
    return _create_line("total", total.v);
}

function summary_line(dates, total) {
    var cnt = 0

    for (var i = 1; i < dates.length; ++i)
	cnt += dates[i].duration;

    total.add(cnt);

    return _create_line(DATE_FMT.format(dates[0]), cnt);
}

var Summary = function(table) {
    'use strict';

    this.__el = document.createElement("div");
    this.__content = document.createElement("div");
    this.__table = table
    this.__tbody = table.getElementsByTagName("tbody");

    if (!this.__tbody || this.__tbody.length != 1)
	// todo: throw exception
	return;

    this.__tbody = this.__tbody[0];

    this.fill = function() {
	var self = this;

	this.__el.classList.add("grid");
	this.__el.appendChild(this.__content);

	this.refresh()
    };

    this.attach = function(parent, next) {
	parent.insertBefore(this.__el, next);
    };

    this.refresh = function() {
	var rows = this.__tbody.getElementsByTagName("tr");
	var dates = Object.create(null);

	this.__content.innerHTML = "ERROR";

	for (var i = 0; i < rows.length; ++i) {
	    var row = rows[i];

	    if (row.classList.contains("filtered"))
		continue;

	    var e = new DateEntry(row);

	    if (!(e.date in dates))
		dates[e.date] = [e.date]

	    dates[e.date].push(e);
	}

	var date_list = []
	for (var d in dates)
	    date_list.push(dates[d]);

	date_list = date_list.sort(function(a,b) {
	    return a[0] - b[0];
	});

	var table = document.createElement("table");
	var tbody = document.createElement("tbody");

	table.appendChild(tbody);

	var last_day = null
	var total = new Counter();

	for (var d in date_list) {
	    var dates = date_list[d];
	    var date = dates[0];

	    if (!last_day)
		last_day = date;
	    else if (!last_day.isSameWeek(date)) {
		tbody.appendChild(hline(2));
		last_day = date;
	    }

	    tbody.appendChild(summary_line(dates, total));
	}

	tbody.appendChild(hline(2));
	tbody.appendChild(total_line(total));
	
	this.__content.innerHTML = "";
	this.__content.appendChild(table);
    }
};

function fixup_xtra_detail()
{
    var h1 = document.getElementsByTagName("h1");
    if (!h1 || h1.length != 1)
	return;
    h1 = h1[0];

    var grid = document.getElementsByClassName("grid");
    if (!grid)
	return;
    grid = grid[0];

    var table = document.getElementById('timeslist');
    if (!table)
	return;

    var summary = new Summary(table);
    summary.attach(grid.parentNode, grid.nextElementSibling);

    fixup_element_single(h1,
			 function(el) {
			     fixup_h1(el, grid, summary);
			 });

    summary.fill();
    grid.hidden = true;

    table.addEventListener("change", function() {
	summary.refresh();
	return true;
    });

    console.log("xtra-detail done");
}

try {
    fixup_xtra_detail();
} catch (err) {
    alert(err);
};
