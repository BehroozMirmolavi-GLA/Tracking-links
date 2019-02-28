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