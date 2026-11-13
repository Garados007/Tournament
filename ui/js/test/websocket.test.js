const { test, beforeEach, afterEach, mock } = require("node:test");
const assert = require("node:assert/strict");
const ElmWebSocketPorts = require("../websocket.js");

// Longer than any backoff delay, including jitter.
const LONG = 30000;

function makeEnv() {
  const env = { sockets: [], online: true };

  class FakeWebSocket {
    static CONNECTING = 0;
    static OPEN = 1;
    static CLOSING = 2;
    static CLOSED = 3;

    constructor(address) {
      this.address = address;
      this.readyState = FakeWebSocket.CONNECTING;
      this.sent = [];
      this.sendTimes = [];
      this.closeCalled = false;
      env.sockets.push(this);
    }

    send(data) {
      if (this.readyState !== FakeWebSocket.OPEN) throw new Error("socket is not open");
      this.sent.push(data);
      this.sendTimes.push(Date.now());
    }

    close() {
      this.closeCalled = true;
      this.readyState = FakeWebSocket.CLOSED;
    }

    // Test controls for the events of the browser socket.
    open() {
      this.readyState = FakeWebSocket.OPEN;
      this.onopen({});
    }

    receive(data) {
      this.onmessage({ data });
    }

    drop(code = 1006, reason = "") {
      this.readyState = FakeWebSocket.CLOSED;
      this.onclose({ code, reason });
    }
  }

  env.WebSocket = FakeWebSocket;
  env.latest = () => env.sockets[env.sockets.length - 1];
  return env;
}

function makeApp(env) {
  const app = { received: [], closes: [], ports: {} };
  let handler;
  app.ports.sendSocketCommand = { subscribe: (h) => (handler = h) };
  app.ports.receiveSocketMsg = { send: (m) => app.received.push(m) };
  app.ports.receiveSocketClose = { send: (m) => app.closes.push(m) };
  app.middleware = new ElmWebSocketPorts({
    WebSocket: env.WebSocket,
    isOnline: () => env.online,
    now: () => Date.now(),
  });
  app.middleware.subscribe(app);
  app.connect = () => handler({ cmd: "connect", name: "ws", address: "ws://relay/ws?id=g" });
  app.send = (content) => handler({ cmd: "send", name: "ws", content });
  app.close = () => handler({ cmd: "close", name: "ws" });
  app.command = (command) => handler(command);
  return app;
}

// Runs the reconnect timer until the middleware creates the next socket.
function nextSocket(env) {
  const count = env.sockets.length;
  for (let waited = 0; waited < LONG && env.sockets.length === count; waited += 50)
    mock.timers.tick(50);
  assert.equal(env.sockets.length, count + 1, "no reconnect");
  return env.latest();
}

let env;
let app;

beforeEach(() => {
  mock.timers.enable({ apis: ["setTimeout", "Date"], now: 1000000 });
  mock.method(console, "log", () => {});
  mock.method(console, "warn", () => {});
  env = makeEnv();
  app = makeApp(env);
});

afterEach(() => {
  mock.timers.reset();
  mock.restoreAll();
});

test("a send while connecting is delivered on open", () => {
  app.connect();
  app.send("a");
  assert.deepEqual(env.latest().sent, []);
  env.latest().open();
  assert.deepEqual(env.latest().sent, ["a"]);
});

test("received messages are passed to Elm", () => {
  app.connect();
  env.latest().open();
  env.latest().receive("hello");
  assert.deepEqual(app.received, [{ name: "ws", msg: "data", data: "hello" }]);
});

test("a burst of sends is coalesced to the latest one", () => {
  app.connect();
  const socket = env.latest();
  socket.open();
  app.send("1");
  app.send("2");
  app.send("3");
  app.send("4");
  assert.deepEqual(socket.sent, ["1"]);
  mock.timers.tick(99);
  assert.deepEqual(socket.sent, ["1"]);
  mock.timers.tick(1);
  assert.deepEqual(socket.sent, ["1", "4"]);
});

test("sends are at least 100 ms apart", () => {
  app.connect();
  const socket = env.latest();
  socket.open();
  for (let i = 0; i < 50; i++) {
    app.send(String(i));
    mock.timers.tick(30);
  }
  mock.timers.tick(LONG);
  assert.equal(socket.sent.at(-1), "49");
  for (let i = 1; i < socket.sendTimes.length; i++)
    assert.ok(socket.sendTimes[i] - socket.sendTimes[i - 1] >= 100, socket.sendTimes.join());
});

test("reconnects after an opened socket closes and sends the last message again", () => {
  app.connect();
  env.latest().open();
  app.send("state");
  env.latest().drop();
  const socket = nextSocket(env);
  assert.equal(socket.address, "ws://relay/ws?id=g");
  socket.open();
  assert.deepEqual(socket.sent, ["state"]);
  assert.deepEqual(app.closes, []);
});

test("a send while disconnected is delivered after the reconnect", () => {
  app.connect();
  env.latest().open();
  app.send("old");
  env.latest().drop();
  app.send("new");
  const socket = nextSocket(env);
  socket.open();
  assert.deepEqual(socket.sent, ["new"]);
});

test("Elm is told only after 3 failed handshakes in a row", () => {
  app.connect();
  env.latest().drop();
  nextSocket(env).drop();
  assert.deepEqual(app.closes, []);
  nextSocket(env).drop(1002, "unknown group");
  assert.deepEqual(app.closes, [{ name: "ws", code: 1002, reason: "unknown group" }]);
  assert.equal(app.middleware.entries.ws, undefined);
  mock.timers.tick(LONG);
  assert.equal(env.sockets.length, 3);
});

test("a successful handshake resets the failed handshakes", () => {
  app.connect();
  env.latest().drop();
  nextSocket(env).drop();
  nextSocket(env).open();
  env.latest().drop();
  nextSocket(env).drop();
  nextSocket(env).drop();
  assert.deepEqual(app.closes, []);
});

test("failed handshakes while offline don't count", () => {
  app.connect();
  env.latest().drop();
  nextSocket(env).drop();
  const socket = nextSocket(env);
  env.online = false;
  socket.drop();
  assert.deepEqual(app.closes, []);
  // Offline, the middleware waits for the online event instead of retrying.
  mock.timers.tick(LONG);
  assert.equal(env.sockets.length, 3);
  env.online = true;
  app.middleware.wakeUp();
  assert.equal(env.sockets.length, 4);
  env.latest().drop();
  assert.equal(app.closes.length, 1);
});

test("connect with the same name replaces the socket", () => {
  app.connect();
  const old = env.latest();
  old.open();
  app.connect();
  const current = env.latest();
  assert.notEqual(current, old);
  assert.ok(old.closeCalled);
  old.receive("stale");
  old.drop(1000);
  mock.timers.tick(LONG);
  assert.equal(env.sockets.length, 2);
  assert.deepEqual(app.closes, []);
  assert.deepEqual(app.received, []);
  current.open();
  app.send("x");
  assert.deepEqual(current.sent, ["x"]);
});

test("a send without a connection doesn't throw", () => {
  assert.doesNotThrow(() => app.command({ cmd: "send", name: "nope", content: "x" }));
});

test("an explicit close neither reconnects nor reports", () => {
  app.connect();
  const socket = env.latest();
  socket.open();
  app.close();
  assert.ok(socket.closeCalled);
  socket.receive("stale");
  socket.drop(1000);
  mock.timers.tick(LONG);
  assert.equal(env.sockets.length, 1);
  assert.deepEqual(app.closes, []);
  assert.deepEqual(app.received, []);
  assert.equal(app.middleware.entries.ws, undefined);
});
