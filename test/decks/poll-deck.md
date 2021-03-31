---
title: Audience Response Poll
poll: 
  email: ""
  server: "polls.hci.informatik.uni-wuerzburg.de:80"
---

# Audience Response Polls

## Build Polls

- Currently supports multiple choice questions
- Add to YAML header: `poll: true`
- To receive email results: 

```.yaml
poll:
  email: <your-email-address>
- Optionally add to YAML header: `poll.ema
```

- Add `{.poll}` to slide header
- Optionally add chart specifications in YAML block on slide


# Audience Response Polls

## Interaction

- Press `c` to display the QR code
- Press `c` again to hide the QR code
- Press `a` to start the poll
- Press `a` again to close the poll
- Timed polls will automatically close


# Example Audience Response Poll {.poll}

## Question  {.qmc .plain}

In which wavelength spectrum of do semiconductor nodes reach litography?

- [x] X-Ray
- [ ] Long radio waves
- [ ] Microwave
- [ ] UV
- [ ] Infrasound
- [ ] 2 nm



# Image {.poll}

## Question  {.qmc}

Name is the name of the image below?

- [ ] Bugs Bunny
- [ ] Hase
- [x] Stanford Bunny
- [ ] Leporidae

```yaml
label: "Votes"
color: "#080"
yAxis:
  stepSize: 2 
  position: "right"
xAxis:
  position: "top"
timed: True
seconds: 15
blink: True
``` 

## 

![](example-stanford-bunny-200x160.jpg)



# No Poll Present

## {.qmc .plain}

In boolean algebra, which value represents true?

- [x] 1
- [ ] 0