# Tournament

Single-page Elm 0.19.3 app that shows a tournament bracket. The editor's
browser sends the complete state to a websocket relay
([Websocket.Relay](https://github.com/Garados007/Websocket.Relay)), which
forwards it to all viewers of the same group (`?id=` in the URL). The relay
deletes a group when its last connection leaves; nothing is persisted
server-side.

## Layout

- `ui/src/` Elm sources, `ui/elm.json`
- `ui/js/` port middleware (`websocket.js`, `local-storage.js`)
- `ui/css/` styles, `index.html` entry page (shared by dev and production)
- `Dockerfile` production build (Elm → uglify → thttpd)

## Commands

Run from `ui/`:

- Build check: `elm make src/Main.elm --output=/dev/null`
- Dev server: `./elm-live.sh` (serves `../index.html`, port 8001)

## Relay

- Production: `https://relay.2complex.de`
- Local: `http://localhost:8005`. The maintainer keeps one instance running
  for all agents. Never start, stop or restart it.

## Conventions

- Match the style of the surrounding code.
- Comments explain only what the code and names can't. Remove comments that
  add nothing.
- Commit messages briefly say *why*; the diff shows *what*.
- No narrative agent thinking in comments, commit messages or PR texts.
- English identifiers, comments and UI texts. The maintainer isn't a native
  speaker; fix spelling mistakes in the code you touch.
- The JSON format on the wire and in localStorage may change freely. No
  migration or backward compatibility is needed.
- `ui/js/websocket.js`: every message is the complete state, so only the latest
  one matters. The relay drops messages arriving within 50 ms of the previous
  one on the same connection.

## Git workflow

- Work in your own worktree under `.claude/worktrees/<name>`.
- Stage files explicitly; never `git add -A` or `git add .`. Untracked files in
  the main checkout (e.g. `docker-compose.yml`) belong to the maintainer: never
  commit, move or delete them.
- Never merge PRs and never push to `master`; the maintainer merges.
- Don't install packages (`npm install`, `pip install`). Use `npm ci`; if a
  dependency is missing or must change, ask the maintainer.
- Stop processes only by their PID, never by name: other agents' processes and
  the shared relay run on the same machine.
- Open PRs yourself and split large changes. Stack dependent PRs with
  `gh stack` (`gh stack init` / `gh stack add <branch>` / `gh stack submit`).
