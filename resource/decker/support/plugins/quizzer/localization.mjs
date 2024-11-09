const german = {
  unableToConnect: "Keine Verbindung zum Quizzer Server möglich.",
  disconnected: "Die Verbindung zum Quizzer Server wurde unterbrochen.",
  unableToGetSession: "Es konnte keine Quizzer Sitzung angefragt werden.",
  unknownError:
    "Es ist ein unbekannter Fehler bei der Datenübertragung aufgetreten.",
  placeholder: "Platzhalter #",
  checkSolution: "Lösung überprüfen",
  errorMissingQuestion: "Fehler: Dieses Quiz besitzt keinen Fragetext.",
  errorMultipleAssignments:
    "Fehler: Ein Zuweisungsquiz darf nur eine Antwortliste besitzen.",
  errorMultipleQuizzes: "Fehler: Eine Folie darf nur ein Quiz beinhalten!",
  errorMissingOptions: "Fehler: Es gibt mehr Platzhalter als Antwortlisten.",
  pickMessage: "Wähle aus ...",
  pickReason: "Bitte wähle eine Antwort aus.",
  assignmentInstructionObjects:
    "Objekte von hier ziehen oder durch Klicken selektieren ...",
  assignmentInstructionCategories: "... und den richtigen Kategorien zuweisen.",
  latency: "Latenz: ",
  showQRCode: "QR-Code anzeigen",
  hideQRCode: "QR-Code verbergen",
  activatePoll: "Umfrage starten",
  evaluate: "Umfrage auswerten",
  clickToClose: "Klicken zum Schließen",
};

const english = {
  unableToConnect: "Unable to connect to quizzer server.",
  disconnected: "Connection to the quizzer server closed.",
  unableToGetSession: "Unable to acquire a quizzer session.",
  unknownError: "An unknown error has occurred during communication.",
  placeholder: "Placeholder #",
  checkSolution: "Check solution",
  errorMissingQuestion: "Error: The quiz is missing a question",
  errorMultipleAssignments:
    "Error: An assignment quiz may only have one list of answers.",
  errorMultipleQuizzes: "Error: A slide may only have a single quiz!",
  errorMissingOptions:
    "Error: You declared more placeholders than lists of answers.",
  pickMessage: "Pick one ...",
  pickReason: "Please pick an answer.",
  assignmentInstructionObjects:
    "Drag & Drop objects or click to select them ...",
  assignmentInstructionCategories:
    "... and assign them to the correct category.",
  latency: "Latency: ",
  showQRCode: "Show QR code",
  hideQRCode: "Hide QR code",
  activatePoll: "Start poll",
  evaluate: "Evaluate poll",
  clickToClose: "Click to close",
};

export default function localization() {
  if (navigator.language === "de") {
    return german;
  } else {
    return english;
  }
}
