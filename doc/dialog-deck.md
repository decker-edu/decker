---
title: How to use dialogs
---

# Enable usage

The deck template needs to contain.

`<script type="module" src="$decker-support-dir$/components/custom-dialog.js"></script>`

wue, mono and mario decks might not contain this line.

# showDialog

`window.showDialog(message, content, options, severity)`

Returns a promise with the response of the user.

- `message`: The message to display at the top of the dialog-box.
- `content`: Optional additional content to display inside the dialog-box.
- `options`: An array of objects containing the keys `text` and `value`.
- `severity`: Decides the color used as the border of the box.

More information about the parameters in the subslides.

# message {.sub}

A short message to be displayed to the user. If the message is longer the dialog box will at max use a certain percentage of the screen. That value right now is half the screen width.

# content {.sub}

Form elements will have their values returned by the callback using their names.
If multiple form elements have the same name, values with the same name will be returned as an array.
The name `submit` is reserved as the result of the option chosen to close the dialog with.

# options {.sub}

The `text` is the text displayed on a button representing the option while the `value` is returned in the `submit`-key of the returned response if that button is pressed.

# severity {.sub}

Possible values are:

- `error`: Red(-ish) using the variable 
- `warning`: Orange(-ish)
- `alert`: Yellow(-ish)
- `information`: Blue(-ish) (default)
- `success`: Green(-ish)

# showInformation

`window.showInformation(message, [content])`

Shorthand for a simple message. Only a button with the label "OK" and the value of "confirmed" will be shown next to the message. Severity is always "information". Content may be specified. Content may be form elements, but the button label being fixed at "OK" is semantically incompatible with "submitting" information.

# showChoice

`window.showChoice(message, options, [severity])`

Shorthand for a simple choice. Shows the message with an array of buttons defined in the options. Severity may be specified. Is "information" by default.