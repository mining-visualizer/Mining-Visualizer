
/*
	This file is part of Mining Visualizer.

	Mining Visualizer is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Mining Visualizer is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with cpp-ethereum.  If not, see <http://www.gnu.org/licenses/>.
*/

var heartBeatMs = null;
var flatline = false;


function readDateVar(element) 
{
	// receive id string of an HTML element, whose textContent contains a valid date/time string
	// return number of milliseconds that the specified date/time occured in the past.
	var el;
	if (el = document.getElementById(element)) 
	{
		var dateTime = parseDate(el.textContent);
		if (isValidDate(dateTime)) 
			return (new Date()) - dateTime; 	// milliseconds 
	}
	return null;
}

function parseDate(str) {
	var m = str.match(/(\d+)[-/.](\d+)[-/.](\d+)\s+(\d+)[-:/.](\d+)[-:/.](\d+)/);
	return new Date(+m[1], +m[2] - 1, +m[3], +m[4], +m[5], +m[6]);
}

function isValidDate(d) 
{
	if (Object.prototype.toString.call(d) === "[object Date]") {
		// it is a date
		if (isNaN(d.getTime()))
			return false;
		else
			return true;
	}
	return false;
}

function checkHeartBeat()
{
	// this gets called once per second by the page. if the 'heartBeat' has not 'beat' in more than 45 seconds,
	// hide the widget contents and display the 'offline' message.
	if (heartBeatMs !== null) 
	{
		heartBeatMs += 1000;
		if (!flatline && heartBeatMs > (45 * 1000))
		{
			document.getElementById("WidgetBody").style.display = "none";
			document.getElementById("Offline").style.display = "block";
			flatline = true;
		}
	}
}

