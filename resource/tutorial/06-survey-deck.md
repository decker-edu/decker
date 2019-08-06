---
title: Dachdecker Survey Overview
dachdecker: 1234asdf-1234asdf 
---

# Introduction


This deck introduces the live survey functionality built into decker and shows how to include a student response system/clicker quiz into the slides.

# Dachdecker

Dachdecker is the server application providing support for clicker quizzes. To use Dachdecker, you need an LDAP account for logging in via `gitlab2.informatik.uni-wuerzburg.de`.

# Workflow

- Navigate to [https://dach.decker.informatik.uni-wuerzburg.de/](https://dach.decker.informatik.uni-wuerzburg.de/)  
- Click `Login`. You will be redirected to gitlab and need to login using your LDAP credentials.
- Returning to the dachdecker page you can now click `Create new Deck` which creates a unique ID.

# 

Include this ID in the `YAML` header of your slide deck 

```yaml
---
dachdecker: 1234asdf-1234asdf 
---
```

In this slide deck you can now create multiple choice questions as seen in the [Quiz deck](./05-quiz-deck.html).


#

- Once you finished creating your presentation, call `decker html` to create the HTML files
- On slides with multiple choice questions, a QR code and link will have been created which leads the students to the poll
- On the vertical slide below the multiple choice question, the results of the poll will be shown.

# Example Poll

* [ ] A
* [ ] B
* [X] C
* [ ] D

#

- Call `decker sync` and enter your LDAP credentials on the command line. This syncs the presentation with the dachdecker server so that the server and the presentation can communicate
- Start `decker server` to start your presentation
- On your presentation, press `q` to log in to dachdecker and activate the questions (the login can be done in advance so you only have to click "Activate Quiz" during the presentation)

# Note:

`dachdecker` is currently not compatible with using `transition: fade` or the revealjs chalkboard plugin. 