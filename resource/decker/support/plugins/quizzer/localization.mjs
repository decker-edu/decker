const german = {
  placeholder: "Platzhalter #",
  checkSolution: "Lösung überprüfen",
  errorMissingQuestion: "Fehler: Dieses Quiz besitzt keinen Fragetext",
};
const english = {
  placeholder: "Placeholder #",
  checkSolution: "Check solution",
  errorMissingQuestion: "Error: The quiz is missing a question",
};

export default function localization() {
  if (navigator.language === "de") {
    return german;
  } else {
    return english;
  }
}
