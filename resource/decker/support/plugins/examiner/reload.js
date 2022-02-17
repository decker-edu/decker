if (location.hostname == "localhost" || location.hostname == "0.0.0.0") {
  let socket = new WebSocket("ws://" + location.host + "/reload");
  socket.onmessage = function (event) {
    if (event.data.startsWith("reload!"))
      window.location.reload();
  };
};

