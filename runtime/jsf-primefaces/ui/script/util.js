const OPTIONAL_LONG = '-9223372036854775808';
const OPTIONAL_INTEGER = '-2147483648';
const OPTIONAL_STRING = '';

// Open the list-of-values dialog
function openLOV(windowName, path, dialogWidth, dialogHeight, idElementId, displayElementId) {
  const top = (screen.height - dialogHeight) / 2;
  const left = (screen.width - dialogWidth) / 2;
  let url = window.location.protocol + "//" + window.location.host + path + '?idElementId=' + idElementId;

  if (displayElementId) {
    url = url + '&displayElementId=' + displayElementId;
  }

  let options = "width=" + dialogWidth + ",height=" + dialogHeight + ",left=" + left;
  options = options + ",top=" + top + ",location=no,menubar=no,toolbar=no";

  // Open the dialog
  window.open(url, windowName, options);
}

// Set the selected item of type string
function setSelectedStringItem() {
  setSelectedItem(OPTIONAL_STRING);
}

// Set the selected item of type integer
function setSelectedIntegerItem() {
  setSelectedItem(OPTIONAL_INTEGER);
}

// Set the selected item of type long
function setSelectedLongItem() {
  setSelectedItem(OPTIONAL_LONG);
}

// Set the selected item in the list-of-values dialog
function setSelectedItem(emptyItemDefaultValue) {
  const idValue = document.getElementById('form:selectedObjectId').value;
  const displayValue = document.getElementById('form:selectedDisplayValue').value;
  const idElementId = document.getElementById('form:idElementId').value;
  const displayElementId = document.getElementById('form:displayElementId').value;

  // Do not apply an invalid value!
  if ((displayElementId && !displayValue) || idValue === emptyItemDefaultValue) {
    return;
  }

  if (window.opener && !window.opener.closed) {
    window.opener.document.getElementById(idElementId).value = idValue;

    if (displayElementId) {
      window.opener.document.getElementById(displayElementId).value = displayValue;
    }

    window.close();
  }
}

// Reset the selected item of type string
function resetSelectedStringItem() {
  resetSelectedItem(OPTIONAL_STRING);
}

// Reset the selected item of type integer
function resetSelectedIntegerItem() {
  resetSelectedItem(OPTIONAL_INTEGER);
}

// Reset the selected item of type long
function resetSelectedLongItem() {
  resetSelectedItem(OPTIONAL_LONG);
}

// Reset the selected item in the list-of-values dialog. As this component is not aware if the
// respective field is mandatory or optional it is always possible to reset the underlying
// association!
function resetSelectedItem(emptyItemDefaultValue) {
  const idElementId = document.getElementById('form:idElementId').value;
  const displayElementId = document.getElementById('form:displayElementId').value;

  if (window.opener && !window.opener.closed) {
    window.opener.document.getElementById(idElementId).value = emptyItemDefaultValue;

    if (displayElementId) {
      window.opener.document.getElementById(displayElementId).value = OPTIONAL_STRING;
    }

    window.close();
  }
}
