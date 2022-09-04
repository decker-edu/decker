---
title: Live Captioning Test Deck

live-captions: true

caption-server: https://decker.cs.tu-dortmund.de/captions
speech-recognition-language: de-DE
---

# How-to

- Start presenter mode by pressing **P** three times in quick succession.
- A []{.fas .fa-closed-captioning} Button should be visible in the top right.
- Click it, be aware of the warning and a popup with a black page should open. Keep it open. You may reposition the popup onto a projection space for your whole audience to see.
- If a caption-server is supplied then a QR-Code will be displayed for your audience to scan.
- Attendees with hearing problems can read your live captioned voice on their mobile device.
- Closing the black window also ends captioning.
- Clicking the button again, now displayed as []{.fas .fa-circle}, re-displays the QR-Code and allows you to stop the captioning.

# Deck Configuration

```
live-captions: true
caption-server: https://decker.cs.tu-dortmund.de/captions
speech-recognition-language: de-DE
```

- `live-captions: true` : Enable the feature in the default template.
- `caption-server` : An address to a running live-captioning sync service.
- `speech-recognition-language` : If not supplied, the WebSpeechAPI uses your browser's default language. If supplied it takes a BCP 47 language tag.

# Language Tag Parsing

As a common user might now be versed in these language codes, the following is also allowed:

- A 2-letter language code
- A 3-letter language code of "deu" or "eng"
- A BCP 47 5-letter language code that matches the RegEx "/^..-../" (two two-letter codes separated by a dash)
- The phrases "Deutsch", "German", "Englisch" or "English"