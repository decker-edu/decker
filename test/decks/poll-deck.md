---
title: Audience Response Poll
poll: true
zoom: false
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
- Click the arrow icon to login to save poll results.
- Press `a` to toggle the poll state opened/closed
- Timed polls will automatically close
- Press `t` to toggle poll results. 
- Drag results box to new location if needed.

## Dashboard

- Open the QR code on the slide deck and click the arrow to login. Poll results will then be saved to your account.
- Access `https://polls.hci.informatik.uni-wuerzburg.de/dash.html` to view poll results.



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

```yaml
color: "#080"
timed: True
seconds: 15
font-color: "red"
font-size: 22
font-style: 'italic'
``` 

# No Poll Present

## {.qmc .plain}

In boolean algebra, which value represents true?

- [x] 1
- [ ] 0

# Interrupts and Polling: Exercise {.poll}
## A busy loop {.qmc .plain}
 
- [X] is an alternative to using an interrupt, e.g., during I/O processing.
   - Yes, but it should be avoided without further delays within the loop, e.g., `sleep(n);`.
- [ ] is a conservative reactive pattern that minimizes CPU load.
   - Actually, it's more the opposite. Without additional measures it needs CPU cycles. 
- [X] also is named spinning sometimes (it continuously spins/loops).
   - Yes, busy looping, busy waiting, or spinning is a technique in which a process repeatedly checks to see if a condition is true [(see here)](https://en.wikipedia.org/w/index.php?title=Busy_waiting&oldid=979011869).
- [ ] can not be modified to use less CPU resources.
   - It can be modified, e.g., by using `sleep(n);` within.
- [X] is a typical pattern used for polling input. 
   - Yes, polling often is used within a busy loop.

# Interrupts: Exercise 2 {.poll}
## Question {.question}
Which of these statements are correct about interrupts?

## {.qmc .plain}
- [ ] Device interrupts can occur even device is not ready yet.
  - Interrupts will signal if the device is ready.
- [x] Interrupts can be asynchronous and synchronous.
- [ ] Synchronous interrupts occur at the same time as the real event and are thus unexpected.
  - Asynchronous events are unexpected.
- [ ] Traps/Exceptions are invoked by external I/O devices.
  - These types of interrupts are internal, e.g., division by zero.

# Quality Criteria for Research {.poll}

## Which are criteria for research? {.qmc .plain}

- [X] Soundness
- [ ] Quantity
- [ ] Reflectiveness