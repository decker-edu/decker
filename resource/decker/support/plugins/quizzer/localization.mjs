const german = {
  unableToConnect: "Keine Verbindung zum Quizzer Server möglich.",
  placeholder: "Platzhalter #",
  checkSolution: "Lösung überprüfen",
  errorMissingQuestion: "Fehler: Dieses Quiz besitzt keinen Fragetext.",
  errorMultipleAssignments:
    "Fehler: Ein Zuweisungsquiz darf nur eine Antwortliste besitzen.",
  pickMessage: "Wähle aus ...",
  pickReason: "Bitte wähle eine Antwort aus.",
  assignmentInstructionObjects:
    "Objekte von hier ziehen oder durch Klicken selektieren ...",
  assignmentInstructionCategories: "... und den richtigen Kategorien zuweisen.",
};

const english = {
  unableToConnect: "Unable to connect to quizzer server.",
  placeholder: "Placeholder #",
  checkSolution: "Check solution",
  errorMissingQuestion: "Error: The quiz is missing a question",
  errorMultipleAssignments:
    "Error: An assignment quiz may only have one list of answers.",
  pickMessage: "Pick one ...",
  pickReason: "Please pick an answer.",
  assignmentInstructionObjects:
    "Drag & Drop objects or click to select them ...",
  assignmentInstructionCategories:
    "... and assign them to the correct category.",
};

export default function localization() {
  if (navigator.language === "de") {
    return german;
  } else {
    return english;
  }
}
