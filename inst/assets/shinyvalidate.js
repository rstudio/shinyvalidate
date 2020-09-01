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
 * This strategy detects a .form-control within the input el, then:
 * (1) toggles .is-invalid on the control (BS4+) or .has-error to the container (BS3) 
 * (2) adds/removes .invalid-feedback (BS4+) or .help-block (BS3)
 * 
 * https://getbootstrap.com/docs/4.4/components/forms/#server-side
 */
const bootstrapStrategy = {
  isBS3: function() {
    return window.jQuery ? window.jQuery.fn.tab.Constructor.VERSION.match(/^3\./) : false;
  },
  findInputControl: function(el) {
    el = $(el);
    const inputControl = el.is(".form-control") ? el : el.find(".form-control");
    return inputControl.length === 0 ? null : inputControl;
  },
  setInvalid: function(el, binding, id, message) {
    const inputControl = this.findInputControl(el);
    if (!inputControl) {
      return false;
    }
    this.isBS3() ?
      inputControl.parent(".form-group").addClass("has-error") :
      inputControl.addClass("is-invalid");
    inputControl.siblings(".shiny-validation-message").remove();
    if (message) {
      const feedbackClass = this.isBS3() ? "help-block" : "invalid-feedback";
      const msgDiv = $(document.createElement("span")).
        addClass([feedbackClass, "shiny-validation-message"]).
        text(message);
      inputControl.after(msgDiv);
    }
    return true;
  },
  clearInvalid: function(el, binding, id) {
    const inputControl = this.findInputControl(el);
    if (!inputControl) {
      return false;
    }
    inputControl.siblings(".shiny-validation-message").remove();
    this.isBS3() ? 
      inputControl.parent(".form-group").removeClass("has-error") : 
      inputControl.removeClass("is-invalid");
    return true;
  }
};
strategies.push(bootstrapStrategy);


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
