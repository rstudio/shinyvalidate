/**
 * When shinyvalidate receives a notification that an id is invalid or valid,
 * it doesn't have a priori knowledge of how to show/hide validation feedback
 * for that id. Instead, we use several strategies that we try in turn; once
 * a strategy succeeds, we stop.
 */
 
 const strategies = [];

/**
 * This strategy depends on jQuery event handlers. Event handlers
 * should call evt.preventDefault() plus either evt.stopPropagation()
 * or evt.stopImmediatePropagation() to signal that they have handled
 * the showing/clearing.
 */
const eventStrategy = {
  setInvalid: function(el, binding, id, message) {
    const e = $.Event("shinyvalidate:show", {
      el: el,
      binding: binding,
      id: id,
      message: message
    });
    $(el).trigger(e);
    
    return e.isDefaultPrevented();
  },
  clearInvalid: function(el, binding, id) {
    const e = $.Event("shinyvalidate:clear", {
      el: el,
      binding: binding,
      id: id
    });
    $(el).trigger(e);
    
    return e.isDefaultPrevented();
  }
};
strategies.push(eventStrategy);

/**
 * This strategy depends on the input binding itself implementing methods for
 * setInvalid/clearInvalid.
 */
const bindingStrategy = {
  setInvalid: function(el, binding, id, message) {
    if (typeof(binding.setInvalid) !== "function") {
      return false;
    }
    binding.setInvalid(el, message);
    return true;
  },
  clearInvalid: function(el, binding, id) {
    if (typeof(binding.clearInvalid) !== "function") {
      return false;
    }
    binding.clearInvalid(el);
    return true;
  }
};
strategies.push(bindingStrategy);

/**
 * This strategy detects .shiny-input-container at or above the el, and uses
 * Bootstrap 3 classes to display validation messages.
 */
const bs3Strategy = {
  findInputContainer: function(el) {
    el = $(el);
    const inputContainer = el.is(".shiny-input-container") ? el : el.parents(".shiny-input-container");
    return inputContainer.length === 0 ? null : inputContainer;
  },
  setInvalid: function(el, binding, id, message) {
    const inputContainer = this.findInputContainer(el);
    if (!inputContainer) {
      return false;
    }
    inputContainer.addClass("has-error");
    inputContainer.children(".shiny-validation-message").remove();
    if (message) {
      const msgDiv = $(document.createElement("span")).
        addClass(["help-block", "shiny-validation-message"]).
        text(message);
      inputContainer.append(msgDiv);
    }
    return true;
  },
  clearInvalid: function(el, binding, id) {
    const inputContainer = this.findInputContainer(el);
    if (!inputContainer) {
      return false;
    }
    inputContainer.removeClass("has-error");
    inputContainer.children(".shiny-validation-message").remove();
    return true;
  }
};
strategies.push(bs3Strategy);

// TODO: Support Bootstrap 4 too

function setInvalid(el, binding, id, message = null) {
  for (var i = 0; i < strategies.length; i++) {
    if (strategies[i].setInvalid(el, binding, id, message)) {
      return;
    }
  }
  console.warn("Don't know how to display input validation feedback for input '" + id + "'. The message was:\n" + message);
}

function clearInvalid(el, binding, id) {
  for (var i = 0; i < strategies.length; i++) {
    if (strategies[i].clearInvalid(el, binding, id)) {
      return;
    }
  }
  console.warn("Don't know how to clear input validation feedback for input '" + id + "'");
}

function getBoundInputsMap() {
  const results = new Map();
  $(".shiny-bound-input").each(function(index, el) {
    const binding = $(el).data("shiny-input-binding");
    if (binding) {
      const id = binding.getId(el);
      results.set(id, {id: id, el: el, binding: binding});
    }
  });
  return results;
}

if (window.Shiny) {
  Shiny.addCustomMessageHandler("validation-jcheng5", function(message) {
    const boundInputsMap = getBoundInputsMap();
    for (const [key, value] of Object.entries(message)) {
      const input = boundInputsMap.get(key);
      if (!input) {
        console.warn("Couldn't perform validation update on input with id '" + key + "': input not found");
        continue;
      }
      
      if (value === null) {
        clearInvalid(input.el, input.binding, input.id);
      } else {
        setInvalid(input.el, input.binding, input.id, value);
      }
    }
  });
}
