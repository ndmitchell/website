
function abstract_show(id)
{
	abstract_change("abstract", id, true);
}

function abstract_hide(id)
{
	abstract_change("abstract", id, false);
}

function bibtex_show(id)
{
	abstract_change("bibtex", id, true);
}

function bibtex_hide(id)
{
	abstract_change("bibtex", id, false);
}

function abstract_change(name, id, show)
{
	document.getElementById(name + "_show_" + id).style.display = (show ? "none" : "");
	document.getElementById(name + "_hide_" + id).style.display = (show ? "" : "none");
	document.getElementById(name + "_text_" + id).style.display = (show ? "" : "none");
}

