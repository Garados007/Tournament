// Port middleware for the Elm package bburdette/websocket.
//
// Every message the app sends is the complete tournament state, so only the
// latest unsent message matters. This lets the middleware coalesce messages,
// which is also needed because the relay drops messages that arrive within
// 50 ms of the previous one on the same connection.
//
// A lost connection is reopened with exponential backoff. The app is only told
// about a close when reconnecting looks hopeless: the relay deletes a group
// when its last connection leaves, and the handshake for a deleted group fails.
var ElmWebSocketPorts = (function () {
  const MIN_SEND_INTERVAL = 100;
  const BACKOFF_START = 500;
  const BACKOFF_MAX = 15000;
  // Failed handshakes in a row (while online) before the app is told.
  const MAX_FAILED_HANDSHAKES = 3;

  class ElmWebSocketPorts {
    constructor(env) {
      // `env` exists for tests; the browser uses its globals.
      env = env || {};
      this.WebSocket = env.WebSocket || window.WebSocket;
      this.isOnline = env.isOnline || (() => navigator.onLine !== false);
      this.now = env.now || Date.now;
      this.entries = {};
    }

    subscribe(app, commandPort, messagePort, closePort) {
      this.messagePort = app.ports[messagePort || "receiveSocketMsg"];
      this.closePort = app.ports[closePort || "receiveSocketClose"];
      app.ports[commandPort || "sendSocketCommand"].subscribe((command) => {
        try {
          this.handle(command);
        } catch (e) {
          // An exception here would surface inside the Elm runtime.
          console.error("websocket command failed", command, e);
        }
      });
      if (typeof window !== "undefined") {
        window.addEventListener("online", () => this.wakeUp());
        document.addEventListener("visibilitychange", () => {
          if (document.visibilityState === "visible") this.wakeUp();
        });
      }
    }

    handle(command) {
      switch (command.cmd) {
        case "connect": this.connect(command.name, command.address); break;
        case "send": this.send(command.name, command.content); break;
        case "close": this.close(command.name); break;
        default: console.warn("unknown websocket command", command);
      }
    }

    connect(name, address) {
      this.close(name);
      const entry = {
        name: name,
        address: address,
        socket: null,
        wasOpen: false,
        failedHandshakes: 0,
        backoff: BACKOFF_START,
        reconnectTimer: null,
        sendTimer: null,
        pending: null,
        lastSent: null,
        lastSendTime: 0,
      };
      this.entries[name] = entry;
      this.open(entry);
    }

    open(entry) {
      let socket;
      entry.reconnectTimer = null;
      try {
        socket = new this.WebSocket(entry.address);
      } catch (e) {
        // Only an invalid address throws; retrying cannot fix that.
        console.error("cannot create websocket", entry.address, e);
        this.giveUp(entry, 0, "cannot create socket");
        return;
      }
      entry.socket = socket;
      let opened = false;

      // Events of a replaced or closed socket must not touch the current entry.
      const isCurrent = () => this.entries[entry.name] === entry && entry.socket === socket;

      socket.onopen = () => {
        if (!isCurrent()) return;
        opened = true;
        entry.failedHandshakes = 0;
        entry.backoff = BACKOFF_START;
        // The last message may have been lost with the old connection.
        if (entry.wasOpen && entry.pending === null && entry.lastSent !== null)
          entry.pending = entry.lastSent;
        entry.wasOpen = true;
        this.flush(entry);
      };
      socket.onmessage = (event) => {
        if (!isCurrent()) return;
        this.messagePort.send({ name: entry.name, msg: "data", data: event.data });
      };
      socket.onclose = (event) => {
        if (!isCurrent()) return;
        entry.socket = null;
        console.log("websocket closed: " + event.code + " " + event.reason);
        if (!opened && this.isOnline()) entry.failedHandshakes += 1;
        if (entry.failedHandshakes >= MAX_FAILED_HANDSHAKES)
          this.giveUp(entry, event.code, event.reason);
        else this.scheduleReconnect(entry);
      };
      // onerror is always followed by onclose, which does the handling.
      socket.onerror = () => {};
    }

    scheduleReconnect(entry) {
      if (entry.reconnectTimer !== null) return;
      const delay = entry.backoff * (0.75 + Math.random() * 0.5);
      entry.backoff = Math.min(entry.backoff * 2, BACKOFF_MAX);
      entry.reconnectTimer = setTimeout(() => {
        if (this.entries[entry.name] !== entry) return;
        // While offline a handshake cannot succeed; `online` wakes us up.
        if (!this.isOnline()) {
          entry.reconnectTimer = null;
          return;
        }
        this.open(entry);
      }, delay);
    }

    // Reconnects immediately after the device comes back online or the page
    // becomes visible again, instead of waiting for the backoff timer.
    wakeUp() {
      for (const entry of Object.values(this.entries)) {
        if (entry.socket !== null) continue;
        clearTimeout(entry.reconnectTimer);
        entry.reconnectTimer = null;
        entry.backoff = BACKOFF_START;
        this.open(entry);
      }
    }

    giveUp(entry, code, reason) {
      this.dispose(entry);
      delete this.entries[entry.name];
      this.closePort.send({ name: entry.name, code: code, reason: reason || "" });
    }

    send(name, content) {
      const entry = this.entries[name];
      if (!entry) {
        console.warn("websocket send without connection", name);
        return;
      }
      entry.pending = content;
      this.flush(entry);
    }

    flush(entry) {
      if (entry.pending === null || entry.sendTimer !== null) return;
      if (!entry.socket || entry.socket.readyState !== this.WebSocket.OPEN) return;
      const wait = entry.lastSendTime + MIN_SEND_INTERVAL - this.now();
      if (wait > 0) {
        entry.sendTimer = setTimeout(() => {
          entry.sendTimer = null;
          this.flush(entry);
        }, wait);
        return;
      }
      entry.socket.send(entry.pending);
      entry.lastSent = entry.pending;
      entry.pending = null;
      entry.lastSendTime = this.now();
    }

    close(name) {
      const entry = this.entries[name];
      if (!entry) return;
      this.dispose(entry);
      delete this.entries[name];
    }

    dispose(entry) {
      clearTimeout(entry.reconnectTimer);
      clearTimeout(entry.sendTimer);
      entry.reconnectTimer = null;
      entry.sendTimer = null;
      if (entry.socket) {
        const socket = entry.socket;
        entry.socket = null;
        try { socket.close(); } catch (e) {}
      }
    }
  }

  return ElmWebSocketPorts;
})();

if (typeof module !== "undefined") module.exports = ElmWebSocketPorts;
