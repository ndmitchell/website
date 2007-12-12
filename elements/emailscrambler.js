// Email Scrambler 0.1
// Copyright 2004 Darren Winsper (darren at winsper.org.uk)

/* This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation; either version 2.1 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

//=====================================================================
// Event Listener
// by Scott Andrew - http://scottandrew.com
// edited by Mark Wubben, <useCapture> is now set to false
//=====================================================================
function addEvent(obj, evType, fn){
        if(obj.addEventListener){
                obj.addEventListener(evType, fn, false);
                return true;
        } else if (obj.attachEvent){
                var r = obj.attachEvent('on'+evType, fn);
                return r;
        } else {
                return false;
        }
}


function createLink(address, object) {
  link = document.createElement("a");
  link.setAttribute("href", "mailto:" + address);
  addressText = document.createTextNode(address);
  link.appendChild(addressText);
  object.replaceChild(link, object.firstChild);
}

function createAddresses() {
  addresses = document.getElementsByTagName("span");
  for (i = 0; i < addresses.length; i++) {
    spanClass = addresses.item(i).className;
    if (spanClass == "es_address") {
      re = /(\w+)\sAT\s((?:\w+\s(?:DOT)?)+)/;
      text = addresses.item(i).firstChild.data;
      address = text.replace(re, "$1@$2")
      dotReplace = /\sDOT\s/gi;
      address = address.replace(dotReplace, ".");
      createLink(address, addresses.item(i));
    }
  }
}

addEvent(window, "load", createAddresses);
