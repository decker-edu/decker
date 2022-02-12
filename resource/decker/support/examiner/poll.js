export {initPolls};

const session = null;

async function initPolls() {
  session = {
    socket: null,
    status: Idle,
  };
}

