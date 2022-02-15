<style>
  p {display: none;}
  .polling p {display: block;}
  div.buttons {display: flex; flex-flow: column nowrap; align-content: stretch;}
  button {
    padding: 0.5em; 
    margin: 0.1em; 
    font-weight: bold; 
    font-size: 200%; 
    color: var(--shade7); 
    border-radius: var(--block-border-radius);
  }
  button[label="A"] {
    background-color: var(--accent0-bbg);
  }
  button[label="B"] {
    background-color: var(--accent1-bbg);
  }
  button[label="C"] {
    background-color: var(--accent2-bbg);
  }
  button[label="D"] {
    background-color: var(--accent3-bbg);
  }
  button[label="E"] {
    background-color: var(--accent4-bbg);
  }
  button[label="F"] {
    background-color: var(--accent5-bbg);
  }
  button[label="G"] {
    background-color: var(--accent6-bbg);
  }
  button[label="H"] {
    background-color: var(--accent7-bbg);
  }
  button:not([disabled])[label="A"]:hover:not(.choice) {
    background-color: var(--accent0-bg);
  }
  button:not([disabled])[label="B"]:hover:not(.choice) {
    background-color: var(--accent1-bg);
  }
  button:not([disabled])[label="C"]:hover:not(.choice) {
    background-color: var(--accent2-bg);
  }
  button:not([disabled])[label="D"]:hover:not(.choice) {
    background-color: var(--accent3-bg);
  }
  button:not([disabled])[label="E"]:hover:not(.choice) {
    background-color: var(--accent4-bg);
  }
  button:not([disabled])[label="F"]:hover:not(.choice) {
    background-color: var(--accent5-bg);
  }
  button:not([disabled])[label="G"]:hover:not(.choice) {
    background-color: var(--accent6-bg);
  }
  button:not([disabled])[label="H"]:hover:not(.choice) {
    background-color: var(--accent7-bg);
  }
  button[label="A"].choice {
    background-color: var(--accent0);
  }
  button[label="B"].choice {
    background-color: var(--accent1);
  }
  button[label="C"].choice {
    background-color: var(--accent2);
  }
  button[label="D"].choice {
    background-color: var(--accent3);
  }
  button[label="E"].choice {
    background-color: var(--accent4);
  }
  button[label="F"].choice {
    background-color: var(--accent5);
  }
  button[label="G"].choice {
    background-color: var(--accent6);
  }
  button[label="H"].choice {
    background-color: var(--accent7);
  }
</style>

![](poll-client.js){.javascript .run}
