// Virdant docs-site "try dock" — boot script.
//
// Creates the persistent "Try Virdant" pill on every docs page and
// lazily loads the real dock bundle from /try/trydock/trydock.js the
// first time the pill is clicked (so normal page views never pay for
// the CodeMirror/xterm bundle, and the SharedWorker — and its ~2.6 MB
// wasm — only spawns on demand; see PLAN.md).
//
// The pill itself stays dumb: after the bundle loads, the bundle owns
// open/close (it re-wires this same pill element).

(function () {
  // html_js_files scripts run in <head>, before <body> exists.
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot);
  } else {
    boot();
  }

  function boot() {
  if (typeof window === 'undefined') return;
  if (document.getElementById('trydock-pill')) return;

  // Not available on mobile at all: no footer button, no auto-reopen,
  // no bundle load.
  if (window.matchMedia && window.matchMedia('(max-width: 768px)').matches) {
    return;
  }

  // The pill lives in a full-width footer bar pinned to the bottom of
  // the page; the square button sits flush at the left edge.
  var bar = document.createElement('div');
  bar.id = 'trydock-bar';

  var pill = document.createElement('button');
  pill.id = 'trydock-pill';
  pill.type = 'button';
  pill.title = 'Open the Virdant playground dock';
  pill.innerHTML = '<span class="trydock-pill-dot"></span>Try Virdant';
  bar.appendChild(pill);
  document.body.appendChild(bar);

  // boot.css makes the theme's .wy-grid-for-nav the page scroller
  // (the root scrollbar must stop at the footer bar, not run behind
  // it).  The browser's default keyboard scrolling targets the now
  // unscrollable root, so forward the scrolling keys ourselves when
  // the focus is on the page itself (not on a control inside it —
  // those scroll their own nearest scroller natively).
  document.addEventListener('keydown', (e) => {
    const t = e.target;
    if (t && t !== document && t !== document.body && t !== document.documentElement) return;
    const grid = document.querySelector('.wy-grid-for-nav');
    if (!grid) return;
    let delta = null;
    if (e.key === 'ArrowDown') delta = 48;
    else if (e.key === 'ArrowUp') delta = -48;
    else if (e.key === 'PageDown' || (e.key === ' ' && !e.shiftKey)) delta = grid.clientHeight * 0.9;
    else if (e.key === 'PageUp' || (e.key === ' ' && e.shiftKey)) delta = -grid.clientHeight * 0.9;
    else if (e.key === 'Home') { grid.scrollTop = 0; e.preventDefault(); return; }
    else if (e.key === 'End') { grid.scrollTop = grid.scrollHeight; e.preventDefault(); return; }
    else return;
    e.preventDefault();
    grid.scrollTop += delta;
  });

  var loaded = false;

  // Load the dock bundle.  Callers that mean "open on load" set
  // window.__trydockBootOpen themselves (the bundle consumes it);
  // virtry.list() loads without it.  Called by the pill click and,
  // if the dock was open on the previous page of this tab,
  // automatically on load (so the dock stays open across in-site
  // navigation).
  function tryLoad() {
    if (loaded) return;
    // No SharedWorker (Safari < 16.4 and the like): use the full
    // playground page instead.  Deliberately not engineered further.
    if (typeof SharedWorker === 'undefined') {
      window.open('/try/', '_blank');
      return;
    }
    loaded = true;
    pill.classList.add('trydock-loading');
    var s = document.createElement('script');
    s.type = 'module';
    s.src = '/try/trydock/trydock.js';
    s.onerror = function () {
      loaded = false;
      pill.classList.remove('trydock-loading');
      window.open('/try/', '_blank');
    };
    document.head.appendChild(s);
  }

  // A click means "open": the bundle opens the dock on load instead
  // of waiting for a second click.
  // A click means "open": the bundle opens the dock on load instead
  // of waiting for a second click.
  pill.addEventListener('click', () => {
    window.__trydockBootOpen = true;
    tryLoad();
  });

  // .. try:: buttons (try_ext.py): delegated click → virtry.load —
  // the buttons carry the project name in data-virtry-project.
  document.addEventListener('click', (e) => {
    const el = e.target.closest('[data-virtry-project]');
    if (el && window.virtry) virtry.load(el.getAttribute('data-virtry-project'));
  });

  // Reopen the dock automatically when navigating (per-tab flag; the
  // bundle clears it when the dock is closed).
  try {
    if (sessionStorage.getItem('virdant-trydock:open') === '1') {
      window.__trydockBootOpen = true;
      tryLoad();
    }
  } catch { /* storage unavailable */ }

  // Programmatic API for docs pages (and their embedded examples):
  // `virtry.load(project)` opens the Try dock on the given seeded
  // project (e.g. `virtry.load('counter')`) and resets it to the
  // example sources unconditionally.  `virtry.list()` resolves to the
  // seeded project names (a Promise — the list lives in the
  // SharedWorker).  When the bundle is already loaded these delegate
  // to it directly; otherwise load is parked in flags the bundle
  // consumes on load, and list loads the bundle first (without
  // opening the panel).  (Without SharedWorker support the pill's
  // fallback opens the full playground in a new tab, which cannot be
  // steered to a project.)
  if (!window.virtry) {
    window.virtry = {
      load: function (project) {
        if (window.__trydock && typeof window.__trydock.loadProject === 'function') {
          window.__trydock.loadProject(project);
          return;
        }
        window.__trydockBootOpen = true;
        window.__trydockBootProject = String(project ?? '');
        tryLoad();
      },

      list: function () {
        if (window.__trydock && typeof window.__trydock.listProjects === 'function') {
          return window.__trydock.listProjects();
        }
        // The seeds live in the bundle's SharedWorker, so listing
        // requires loading it.  Without SharedWorker support there is
        // nothing to ask (the pill's fallback is a fresh playground
        // tab); reject instead of hanging or opening a tab.
        if (typeof SharedWorker === 'undefined') {
          return Promise.reject(new Error('virtry.list: SharedWorker unavailable'));
        }
        return new Promise((resolve) => {
          const waiters = (window.__trydockBootListWaiters ??= []);
          waiters.push(resolve);
          tryLoad();
        });
      },

      // `virtry.goto_source(target)`: open the dock and jump to a
      // definition.  `target` is either an item FQN (e.g.
      // `counter::Top`) or `<FILENAME>:<LINENO>` (e.g. `top.vir:12`).
      // Resolves once the editor has jumped; rejects — the async
      // form of throwing — when the target cannot be resolved
      // (unknown file or FQN).  Like load/list, a call before the
      // bundle has loaded parks itself (target + promise pair) in a
      // queue the bundle consumes on load; the promise settles then.
      goto_source: function (target) {
        if (window.__trydock && typeof window.__trydock.gotoSource === 'function') {
          return window.__trydock.gotoSource(target);
        }
        const p = new Promise((resolve, reject) => {
          (window.__trydockBootGotoQueue ??= []).push({
            target: String(target ?? ''),
            resolve,
            reject,
          });
        });
        tryLoad();
        return p;
      },

      // `virtry.find_source(target)`: the pure query form of
      // goto_source — same target forms, resolves to true iff
      // goto_source(target) would succeed (false otherwise).  Never
      // opens the dock, never rejects.  Parking works like
      // goto_source's, minus the dock opening.
      find_source: function (target) {
        if (window.__trydock && typeof window.__trydock.findSource === 'function') {
          return window.__trydock.findSource(target);
        }
        const p = new Promise((resolve) => {
          (window.__trydockBootFindQueue ??= []).push({
            target: String(target ?? ''),
            resolve,
          });
        });
        tryLoad();
        return p;
      },
    };
  }
  }
})();
