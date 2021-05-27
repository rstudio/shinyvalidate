"use strict";

function _slicedToArray(arr, i) { return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray(arr, i) || _nonIterableRest(); }

function _nonIterableRest() { throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

function _iterableToArrayLimit(arr, i) { var _i = arr && (typeof Symbol !== "undefined" && arr[Symbol.iterator] || arr["@@iterator"]); if (_i == null) return; var _arr = []; var _n = true; var _d = false; var _s, _e; try { for (_i = _i.call(arr); !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"] != null) _i["return"](); } finally { if (_d) throw _e; } } return _arr; }

function _arrayWithHoles(arr) { if (Array.isArray(arr)) return arr; }

/**
 * When shinyvalidate receives a notification that an id is invalid or valid,
 * it doesn't have a priori knowledge of how to show/hide validation feedback
 * for that id. Instead, we use several strategies that we try in turn; once
 * a strategy succeeds, we stop.
 */
var strategies = [];
/**
 * This strategy depends on jQuery event handlers. Event handlers
 * should call evt.preventDefault() plus either evt.stopPropagation()
 * or evt.stopImmediatePropagation() to signal that they have handled
 * the showing/clearing.
 */

var eventStrategy = {
  setInvalid: function setInvalid(el, binding, id, data) {
    var e = $.Event("shinyvalidate:show", $.extend({
      el: el,
      binding: binding,
      id: id
    }, data));
    $(el).trigger(e);
    return e.isDefaultPrevented();
  },
  clearInvalid: function clearInvalid(el, binding, id) {
    var e = $.Event("shinyvalidate:clear", {
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

var bindingStrategy = {
  setInvalid: function setInvalid(el, binding, id, data) {
    if (typeof binding.setInvalid !== "function") {
      return false;
    }

    binding.setInvalid(el, data);
    return true;
  },
  clearInvalid: function clearInvalid(el, binding, id) {
    if (typeof binding.clearInvalid !== "function") {
      return false;
    }

    binding.clearInvalid(el);
    return true;
  }
};
strategies.push(bindingStrategy);
/**
 * This strategy detects .shiny-input-container at or above the el, and uses
 * Bootstrap 3 & 4 classes to display validation messages.
 */

var bsStrategy = {
  isBS3: function isBS3() {
    if (!$.fn.tab) {
      return false;
    }

    return $.fn.tab.Constructor.VERSION.match(/^3\./);
  },
  findInputContainer: function findInputContainer(el) {
    el = $(el);
    var inputContainer = el.is(".form-group") ? el : el.parents(".form-group");
    return inputContainer.length === 0 ? null : inputContainer;
  },
  setInvalid: function setInvalid(el, binding, id, data) {
    if (data.type !== "error") {
      return false;
    }

    var inputContainer = this.findInputContainer(el);

    if (!inputContainer) {
      return false;
    }

    if (this.isBS3()) {
      inputContainer.addClass("has-error");
    } else {
      // BS4 wants .is-invalid on a .form-control (e.g., <input class="form-control">)
      // *and* wants it to be a _sibling_ of .invalid-message in order to be displayed. 
      //
      // Unfortunately, we can't always assume that .form-control exists 
      // (it conflicts with selectize CSS), so in the event that it's missing , 
      // we fallback to putting is-invalid on the container, which should be compatible 
      // with Selectize + BS4 https://github.com/rstudio/shiny/blob/2bd158a4/inst/www/shared/selectize/scss/selectize.bootstrap4.scss#L131-L140
      var control = inputContainer.find(".form-control");

      if (control.length) {
        control.addClass("is-invalid");
      } else {
        inputContainer.addClass("is-invalid");
      }
    }

    inputContainer.children(".shiny-validation-message").remove();

    if (data.message) {
      var feedbackClass = this.isBS3() ? "help-block" : "invalid-feedback";
      var msg = $(document.createElement("span")).addClass([feedbackClass, "shiny-validation-message"]).text(data.message); // Yes, this is a terrible hack to get feedback to display when 
      // there is no .form-control in BS4

      msg.attr('style', function (i, s) {
        return (s || '') + 'display: block !important;';
      });
      inputContainer.append(msg);
    }

    return true;
  },
  clearInvalid: function clearInvalid(el, binding, id) {
    var inputContainer = this.findInputContainer(el);

    if (!inputContainer) {
      return false;
    }

    if (this.isBS3()) {
      inputContainer.removeClass("has-error");
    } else {
      var control = inputContainer.find(".form-control");

      if (control.length) {
        control.removeClass("is-invalid");
      } else {
        inputContainer.removeClass("is-invalid");
      }
    }

    inputContainer.children(".shiny-validation-message").remove();
    return true;
  }
};
strategies.push(bsStrategy);

function setInvalid(el, binding, id) {
  var data = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : null;

  for (var i = 0; i < strategies.length; i++) {
    if (strategies[i].setInvalid(el, binding, id, data)) {
      return;
    }
  }

  console.warn("Don't know how to display input validation feedback for input '" + id + "'. The message was:\n" + JSON.stringify(data));
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
  var results = new Map();
  $(".shiny-bound-input").each(function (index, el) {
    var binding = $(el).data("shiny-input-binding");

    if (binding) {
      var id = binding.getId(el);
      results.set(id, {
        id: id,
        el: el,
        binding: binding
      });
    }
  });
  return results;
}

if (window.Shiny) {
  Shiny.addCustomMessageHandler("validation-jcheng5", function (message) {
    var boundInputsMap = getBoundInputsMap();

    for (var _i = 0, _Object$entries = Object.entries(message); _i < _Object$entries.length; _i++) {
      var _Object$entries$_i = _slicedToArray(_Object$entries[_i], 2),
          key = _Object$entries$_i[0],
          value = _Object$entries$_i[1];

      var input = boundInputsMap.get(key);

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

