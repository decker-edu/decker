import puppeteer from "puppeteer";

const browser = await puppeteer.launch({ headless: false });
const page = await browser.newPage();

setTimeout(async () => {
  console.log("PERFORMING EXPORT");

  await page.goto(process.argv[2]);
  await page.evaluate(() => {
    return new Promise((resolve) => {
      const reveal = document.querySelector(".reveal");
      reveal.addEventListener("pdf-ready", () => {
        resolve();
      });
    });
  });

  await page.pdf({
    path: process.argv[3],
    displayHeaderFooter: false,
    preferCSSPageSize: true,
  });

  console.log("Finished");
  //await browser.close();
}, 10000);
