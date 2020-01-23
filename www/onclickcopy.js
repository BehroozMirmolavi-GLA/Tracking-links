function onclickcopy() {
  /* Get the text field */
  var copyText = document.getElementById("newurlshort");

  /* Select the text field */
  copyText.select();

  /* Copy the text inside the text field */
  document.execCommand("copy");
alert("Copied the text: " + copyText.value);
}

function onclickcopy2() {
  /* Get the text field */
  var copyText = document.getElementById("newurllong");

  /* Select the text field */
  copyText.select();

  /* Copy the text inside the text field */
  document.execCommand("copy");
alert("Copied the text: " + copyText.value);
}

function idleTimer() {
  var t = setTimeout(logout, 600000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions

  function logout() {
	window.alert("Sorry - you've been timed out due to inactivity");
    // send message to Shiny
    Shiny.onInputChange("timeout", t);
  }

  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, 600000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();