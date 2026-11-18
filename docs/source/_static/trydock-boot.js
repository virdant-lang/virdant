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

  var pill = document.createElement('button');
  pill.id = 'trydock-pill';
  pill.type = 'button';
  pill.title = 'Open the Virdant playground dock';
  pill.innerHTML = '<span class="trydock-pill-dot"></span>Try Virdant';

  var loaded = false;

  // Load the dock bundle; called by the pill click and, if the dock
  // was open on the previous page of this tab, automatically on load
  // (so the dock stays open across in-site navigation).
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
    // Tell the bundle this click already meant "open": it opens the
    // dock on load instead of waiting for a second click.
    window.__trydockBootOpen = true;
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

  pill.addEventListener('click', tryLoad);

  // Reopen the dock automatically when navigating (per-tab flag; the
  // bundle clears it when the dock is closed).
  try {
    if (sessionStorage.getItem('virdant-trydock:open') === '1') tryLoad();
  } catch { /* storage unavailable */ }

  document.body.appendChild(pill);
  }
})();
