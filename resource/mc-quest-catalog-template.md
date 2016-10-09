# Aufgabe N: {{Title}}

|               |                   |
|---------------+-------------------|
| Titel         | **{{Title}}**  |
| Id            | {{Id}}         |
| Vorlesung     | {{Lecture}}    |
| Schwierigkeit | {{Difficulty}} |
| Punkte        | {{Points}}     |
| Kommentar     | {{Comment}}    |

{{Question}}

*Antworten:*

{{#Answer.Correct}}
- $\boxtimes$ {{.}}
{{/Answer.Correct}}
{{#Answer.Incorrect}}
- $\square$ {{.}}
{{/Answer.Incorrect}}
