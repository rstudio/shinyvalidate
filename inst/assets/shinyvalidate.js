function findInputContainer(id) {
  // TODO: Should be based on InputBinding.getId
  const el = $("#" + $.escapeSelector(id));
  if (el.length === 0) {
    console.warn("Couldn't set validation message on #" + id + "; element was not found");
    return;
  }
  const inputContainer = el.is(".shiny-input-container") ? el : el.parents(".shiny-input-container");
  if (inputContainer.length === 0) {
    console.warn("Couldn't set validation message on #" + id + "; element was not contained in a .shiny-input-container");
    return;
  }
  return inputContainer;
}

// TODO: Support Bootstrap 4 too
function setInvalid(id, message = null) {
  const inputContainer = findInputContainer(id);
  if (!inputContainer) {
    return;
  }
  inputContainer.addClass("has-error");
  inputContainer.children(".shiny-validation-message").remove();
  if (message) {
    const msgDiv = $(document.createElement("span")).
      addClass(["help-block", "shiny-validation-message"]).
      text(message);
    inputContainer.append(msgDiv);
  }
}

function clearInvalid(id) {
  const inputContainer = findInputContainer(id);
  if (!inputContainer) {
    return;
  }
  inputContainer.removeClass("has-error");
  inputContainer.children(".shiny-validation-message").remove();
}

if (window.Shiny) {
  Shiny.addCustomMessageHandler("validation-jcheng5", function(message) {
    for (const [key, value] of Object.entries(message)) {
      if (value === null) {
        clearInvalid(key);
      } else {
        setInvalid(key, value);
      }
    }
  });
}
