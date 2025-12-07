# Chrome pdf export request explanations

## Preparation

Open chrome in headless mode:

```sh
chrome --headless --remote-debugging-port=9222
```

## Open url

HTTP/PUT <http://localhost:9222/json/new?{url}>

RESPONSE:

```json
{
  "description": "",
  "devtoolsFrontendUrl": "https://chrome-devtools-frontend.appspot.com/serve_rev/@1c008349f76ff3a317bf28316fc5008c0120deb4/inspector.html?ws=127.0.0.1:9222/devtools/page/605A2EA05D6D3F3EE58B6A1FCE65FF76",
  "id": "605A2EA05D6D3F3EE58B6A1FCE65FF76",
  "title": "",
  "type": "page",
  "url": "http://localhost:8888/mmi-10-deck.html?print-pdf",
  "webSocketDebuggerUrl": "ws://127.0.0.1:9222/devtools/page/605A2EA05D6D3F3EE58B6A1FCE65FF76"
}
```

## Activate URL SIDE

HTTP/GET <http://localhost:9222/json/activate/{response.id}>

RESPONSE:

`Target activated`

## Connect to websocket

ENDPOINT: <http://localhost:9222/devtools/page/{response.id}>

## Wait script execution

REQUEST:

```json
{
  "method": "Runtime.callFunctionOn",
  "params": {
    "functionDeclaration": "() => {\n    return new Promise((resolve) => {\n      const reveal = document.querySelector(\".reveal\");\n      reveal.addEventListener(\"pdf-ready\", () => {\n        resolve();\n      });\n    });\n  }\n//# sourceURL=pptr:evaluate;Timeout._onTimeout%20(file%3A%2F%2F%2Fhome%2Ffabius%2FDocuments%2F01-projects%2Funi%2Fpg%2Ftest%2Ftest-pdf-generator%2Ftest.js%3A10%3A14)\n",
    "executionContextId": 3,
    "arguments": [],
    "returnByValue": true,
    "awaitPromise": true,
    "userGesture": true
  },
  "id": 24,
  "sessionId": "D1E0B53CB60A6E5202E209B6557CCB66"
}
```

RESPONSE:

```json
{
  "id": 24,
  "result": {
    "result": {
      "type": "undefined"
    }
  },
  "sessionId": "D1E0B53CB60A6E5202E209B6557CCB66"
}
```

## RANDOM REQUEST THAT MAY BE IMPORTANT IN BETWEEN

REQUESTS:

```json
{
  "method": "Runtime.releaseObject",
  "params": {
    "objectId": "-1208378224821804998.3.1"
  },
  "id": 25,
  "sessionId": "D1E0B53CB60A6E5202E209B6557CCB66"
}
```

```json
{
  "method": "Runtime.callFunctionOn",
  "params": {
    "functionDeclaration": "() => {\n                return document.fonts.ready;\n            }\n//# sourceURL=pptr:evaluate;CdpPage.createPDFStream%20(file%3A%2F%2F%2Fhome%2Ffabius%2Fnode_modules%2Fpuppeteer-core%2Flib%2Fesm%2Fpuppeteer%2Fcdp%2FPage.js%3A804%3A18)\n",
    "executionContextId": 4,
    "arguments": [],
    "returnByValue": true,
    "awaitPromise": true,
    "userGesture": true
  },
  "id": 26,
  "sessionId": "D1E0B53CB60A6E5202E209B6557CCB66"
}
```

RESPONSES:

```json
{
  "id": 25,
  "result": {},
  "sessionId": "D1E0B53CB60A6E5202E209B6557CCB66"
}
```

```json
{
  "id": 26,
  "result": {
    "result": {
      "type": "object",
      "value": {}
    }
  },
  "sessionId": "D1E0B53CB60A6E5202E209B6557CCB66"
}
```

## PDF EXPORT

REQUEST:

```json
{
  "method": "Page.printToPDF",
  "params": {
    "transferMode": "ReturnAsStream",
    "landscape": false,
    "displayHeaderFooter": false,
    "headerTemplate": "",
    "footerTemplate": "",
    "printBackground": false,
    "scale": 1,
    "paperWidth": 8.5,
    "paperHeight": 11,
    "marginTop": 0,
    "marginBottom": 0,
    "marginLeft": 0,
    "marginRight": 0,
    "pageRanges": "",
    "preferCSSPageSize": true,
    "generateTaggedPDF": true,
    "generateDocumentOutline": false
  },
  "id": 27,
  "sessionId": "D1E0B53CB60A6E5202E209B6557CCB66"
}
```

RESPONSE:

```json
{
  "id": 27,
  "result": {
    "data": "",
    "stream": "1"
  },
  "sessionId": "D1E0B53CB60A6E5202E209B6557CCB66"
}
```

## Get PDF FROM BROWSER

```json
{
  "method": "IO.read",
  "params": { "handle": "1" },
  "id": 28,
  "sessionId": "D1E0B53CB60A6E5202E209B6557CCB66"
}
```

Response:

```json
{
  "id": 28,
  "result": {
    "base64Encoded": true,
    "data": "some base64Encoded data",
    "eof": false
  },
  "sessionId": "D1E0B53CB60A6E5202E209B6557CCB66"
}
```

## Cleanup (Close url)

HTTP/GET <http://localhost:9222/json/close/{response.id}>
