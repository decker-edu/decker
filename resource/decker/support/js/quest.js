if (location.hostname == "localhost" || location.hostname == "0.0.0.0") {
  let source = new EventSource("/reload");
  source.onmessage = function (event) {
    if (event.data.startsWith("reload!"))
      window.location.reload();
  };
};
