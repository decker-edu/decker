---
title: Audience Response Poll
poll: true
---

# Audience Response Polls

## Build Polls

- Currently supports multiple choice questions
- Add to YAML header: `poll: true`
- Add `{.poll}` to slide header
- Optionally add chart specifications in YAML block on slide


# Audience Response Polls

## Interaction

- Press `c` to toggle the QR code
- Press `a` to toggle the poll state opened/closed
- Timed polls will automatically close

# Which of the following countries is the place of origin of the Border Collie? {.poll}

## {.qmc .plain}

- [x] Anglo-Scottish Border
- [ ] America/Canada Border
- [ ] Canada
- [ ] Russia
- [ ] Australia


# Which of these Border Collies was owned by Queen Victoria? {.poll}

## {.qmc .plain}

- [ ] Dutch
- [ ] Chaser
- [x] Sharp
- [ ] Rusty

# Image {.poll}

## Question {.qmc}

Which is the name of the image you see below?

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
``` 

## {.center}

![](example-stanford-bunny-200x160.jpg)

# No Poll Present

## {.qmc .plain}

In boolean algebra, which value represents true?

- [x] 1
- [ ] 0