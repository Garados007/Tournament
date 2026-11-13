// Manual integration test of js/websocket.js against a real Websocket.Relay.
//
// Needs the shared relay on http://localhost:8005; this script never starts or
// stops it. Run from `ui/`:
//
//     node js/manual/websocket-relay.js
//
// A local TCP proxy in front of the relay simulates a network drop. The test
// isn't part of `npm test` or CI and takes a few seconds.
const net = require("net");
const ElmWebSocketPorts = require("../websocket.js");

const RELAY_PORT = 8005;
const BASE = "http://localhost:" + RELAY_PORT;
const WS = "ws://localhost:" + RELAY_PORT + "/ws?id=";

// Proxy whose connections can be cut to simulate a network drop.
const proxySockets = new Set();
const proxy = net.createServer((client) => {
  const upstream = net.connect(RELAY_PORT, "localhost");
  client.pipe(upstream);
  upstream.pipe(client);
  const pair = [client, upstream];
  proxySockets.add(pair);
  const end = () => {
    proxySockets.delete(pair);
    client.destroy();
    upstream.destroy();
  };
  client.on("error", end);
  upstream.on("error", end);
  client.on("close", end);
  upstream.on("close", end);
});
const cutNetwork = () => {
  for (const [client, upstream] of proxySockets) {
    client.resetAndDestroy();
    upstream.destroy();
  }
};

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Fake Elm app with the three ports.
function makeApp() {
  const app = { received: [], closes: [], ports: {} };
  let handler;
  app.ports.sendSocketCommand = { subscribe: (h) => (handler = h) };
  app.ports.receiveSocketMsg = { send: (m) => app.received.push(JSON.parse(m.data).value) };
  app.ports.receiveSocketClose = { send: (m) => app.closes.push(m) };
  app.cmd = (c) => handler(c);
  const ports = new ElmWebSocketPorts({ WebSocket, isOnline: () => true });
  ports.subscribe(app);
  app.middleware = ports;
  return app;
}
const relayMsg = (token, value) => JSON.stringify({ $type: "Relay", token, value });

let failures = 0;
function check(name, cond, info) {
  console.log((cond ? "PASS " : "FAIL ") + name + (cond ? "" : " " + JSON.stringify(info)));
  if (!cond) failures++;
}

async function newGroup(query) {
  try {
    return await (await fetch(BASE + "/api/new" + (query || ""))).json();
  } catch (e) {
    console.error("The relay on " + BASE + " is not reachable. Ask the maintainer to start it.");
    process.exit(2);
  }
}

(async () => {
  await new Promise((r) => proxy.listen(0, "localhost", r));
  const proxyWs = "ws://localhost:" + proxy.address().port + "/ws?id=";

  const { id, token } = await newGroup("?replay-last=true");

  const editor = makeApp();
  const viewer = makeApp();
  editor.cmd({ cmd: "connect", name: "ws", address: proxyWs + encodeURIComponent(id) });
  // Sent before the socket is open: must be queued, not thrown away.
  editor.cmd({ cmd: "send", name: "ws", content: relayMsg(token, 1) });
  viewer.cmd({ cmd: "connect", name: "ws", address: WS + encodeURIComponent(id) });
  await sleep(500);
  check("message sent while connecting arrives", viewer.received.includes(1), viewer.received);

  // Burst faster than the relay's 50 ms limit: the latest state must win.
  for (let i = 2; i <= 6; i++) editor.cmd({ cmd: "send", name: "ws", content: relayMsg(token, i) });
  await sleep(500);
  check("latest message of a burst arrives", viewer.received.at(-1) === 6, viewer.received);

  // Connection loss while the group stays alive (viewer still connected).
  const before = Date.now();
  cutNetwork();
  editor.cmd({ cmd: "send", name: "ws", content: relayMsg(token, 7) });
  for (let i = 0; i < 40 && viewer.received.at(-1) !== 7; i++) await sleep(100);
  console.log("     recovered after " + (Date.now() - before) + " ms");
  check("reconnects to the same group", editor.middleware.entries.ws?.socket?.readyState === 1, null);
  check("message sent while disconnected arrives", viewer.received.at(-1) === 7, viewer.received);
  check("no close reported to Elm", editor.closes.length === 0, editor.closes);

  // Sending to an unknown name must not throw into the Elm runtime.
  let threw = false;
  try {
    editor.cmd({ cmd: "send", name: "nope", content: "x" });
  } catch {
    threw = true;
  }
  check("send without connection does not throw", !threw, null);

  // Replacing a connection: the old socket's close must not remove the new one.
  viewer.cmd({ cmd: "connect", name: "ws", address: WS + encodeURIComponent(id) });
  await sleep(500);
  check("reconnect command keeps the new socket", viewer.middleware.entries.ws?.socket?.readyState === 1, null);
  check("replacing a socket reports no close", viewer.closes.length === 0, viewer.closes);

  // The handshake for an unknown group fails like the one for a deleted group,
  // so the app must be told after a few attempts.
  const lost = makeApp();
  const unknownId = "unknown-" + Date.now() + "-" + Math.random().toString(36).slice(2);
  lost.cmd({ cmd: "connect", name: "ws", address: WS + encodeURIComponent(unknownId) });
  for (let i = 0; i < 100 && lost.closes.length === 0; i++) await sleep(100);
  check("gives up when the group doesn't exist", lost.closes.length === 1 && !lost.middleware.entries.ws, lost.closes);

  // Explicit close: no reconnect, no report.
  const { id: id2 } = await newGroup();
  const third = makeApp();
  third.cmd({ cmd: "connect", name: "ws", address: WS + encodeURIComponent(id2) });
  await sleep(300);
  third.cmd({ cmd: "close", name: "ws" });
  await sleep(1500);
  check("explicit close does not reconnect or report", !third.middleware.entries.ws && third.closes.length === 0, third.closes);

  for (const app of [editor, viewer, lost, third]) app.cmd({ cmd: "close", name: "ws" });
  proxy.close();
  console.log(failures === 0 ? "ALL PASSED" : failures + " FAILED");
  process.exit(failures === 0 ? 0 : 1);
})();
