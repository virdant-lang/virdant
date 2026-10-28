BASEDIR = $(abspath ../../../..)
VIR ?= $(BASEDIR)/target/release/vir

# The patterns.vir source does not yet check cleanly:
#   * false-positive "Redundant else arm" for constructor patterns with
#     literal sub-patterns (`case @Pair(true, false)` + `else`) — this is
#     the exact example documented in .agents/skills/virdant/types-and-patterns.md
#   * ident patterns (`case y => y`) are reported non-exhaustive on Word subjects
# The test was never wired in (it was a `sleep 0` stub) precisely because of
# this. Skip with a notice until the pattern matcher is fixed.
# See TESTING_REPORT/NOTES.md.
test:
	@echo "SKIP: patterns — source hits known pattern-matching issues; see TESTING_REPORT/NOTES.md"
