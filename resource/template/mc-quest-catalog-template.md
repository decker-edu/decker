\newpage

# Aufgabe: {{&Title}}

|               |                |
|---------------|----------------|
| Titel         | **{{&Title}}**  |
| Id            | {{&TopicId}}    |
| Vorlesung     | {{&LectureId}}  |
| Schwierigkeit | {{&Difficulty}} |
| Punkte        | {{&Points}}     |
| Kommentar     | {{&Comment}}    |

{{&Question}}

{{\#Answer.Choices}}

-   {{\#Correct}}$\boxtimes${{/Correct}}{{\^Correct}}$\square${{/Correct}} {{&TheAnswer}}

{{/Answer.Choices}}
