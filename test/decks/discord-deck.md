---
title: Discord
---

# Discord is COOL

<button id="discord-button" style="font-size:200%;">Dang!</button>

<script type="module">
  let hook = "https://discord.com/api/webhooks/870347006458101810/qP_QoO0ecp6dJydNWuDvo-zlrsplItxLD_aJorReAGwDCwkhx0AA9a-HP_CHp2mVZAmB"
  let data = {
    username: "The Discord Deck",
    content: "Dong!"
    }

  let button = document.getElementById("discord-button");
  button.addEventListener("click", (e) => {
    fetch(hook, { 
      method: "POST",     
      headers: {
       'Content-Type': 'application/json'
      },
      body: JSON.stringify(data) 
    })
      .then((r) => r.ok)
      .catch((e) => {
        console.log("[] cannot upload form data to: " + "/upload" + ", " + e);  return false;
      });
  });
</script>


