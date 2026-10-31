//! Propagation worklist with O(1) membership and LIFO pops, no hashing.
//!
//! Replaces the `IndexSet<SignalId>` that used to track dirty signals:
//! a bitmap records "in the worklist" in one bit per signal, and a
//! stack records the pop order.  `pop` reproduces `IndexSet::pop`
//! byte-for-byte: the most recently inserted signal pops first, and a
//! signal already in the worklist keeps its original position instead
//! of being re-pushed to the back.

use crate::analysis::elaboration::SignalId;

const WORD_BITS: usize = 64;

/// The `flow` worklist: which signals have pending changes to
/// propagate, and in what order `flow` pops them.
pub(super) struct DirtyList {
    /// One bit per `SignalId::index()`.  The single source of truth for
    /// "in the worklist" — `stack` is derived from it, never consulted
    /// for membership.
    bits: Vec<u64>,
    /// `SignalId::index()` of every signal currently in the worklist,
    /// in insertion order, so `pop` is LIFO.
    stack: Vec<u32>,
}

impl DirtyList {
    pub(super) fn new(n_signals: usize) -> DirtyList {
        DirtyList {
            bits: vec![0; n_signals.div_ceil(WORD_BITS)],
            stack: Vec::new(),
        }
    }

    /// Mark `signal_id` dirty.  Idempotent: a signal already in the
    /// worklist is not re-pushed, so it keeps its original LIFO
    /// position (this is what guarantees `flow` termination — `set_val`
    /// only re-dirties on an actual value change, and an in-flight
    /// signal is not re-queued for a same-value rewrite).
    pub(super) fn insert(&mut self, signal_id: SignalId) {
        let index = signal_id.index();
        let word = index / WORD_BITS;
        let bit = index % WORD_BITS;
        if self.bits[word] & (1 << bit) != 0 {
            return;
        }
        self.bits[word] |= 1 << bit;
        self.stack.push(index as u32);
    }

    /// The number of signals currently in the worklist.
    pub(super) fn len(&self) -> usize {
        self.stack.len()
    }

    /// Pop the most recently inserted still-present signal, or `None`
    /// when the worklist is empty.  Order is identical to
    /// `IndexSet::pop` on the old dirty set.
    pub(super) fn pop(&mut self) -> Option<SignalId> {
        let index = self.stack.pop()? as usize;
        let word = index / WORD_BITS;
        let bit = index % WORD_BITS;
        self.bits[word] &= !(1 << bit);
        Some(SignalId::from_index(index))
    }

    /// The worklist's insertion-order stack (most recent last): the
    /// settlement-entry sequence the cached straight-line schedule keys
    /// off.  Read-only view for the cache's signature computation.
    pub(super) fn stack(&self) -> &[u32] {
        &self.stack
    }

    /// The dirty-set bitmap: one bit per signal index, set exactly for
    /// the signals currently in the worklist.  Read at settlement
    /// entry (before any pops), where it is the settlement's dirty
    /// set — the deep-cycle replay's soundness key.  Consumers trim
    /// with `top_words` (a backward scan, off the hot path).
    pub(super) fn bits(&self) -> &[u64] {
        &self.bits
    }

    /// True when `signal_id` is currently in the worklist.
    ///
    /// No runtime caller today (the watcher-sweep pre-filter that would
    /// consume it was dropped from v1); the differential test below is
    /// what exercises it.
    #[cfg_attr(not(test), allow(dead_code))]
    pub(super) fn contains(&self, signal_id: SignalId) -> bool {
        let index = signal_id.index();
        self.bits[index / WORD_BITS] & (1 << (index % WORD_BITS)) != 0
    }

    /// Drop every signal from the worklist (after `flow_all`, which
    /// propagates the whole network by explicit iteration rather than
    /// by draining).
    pub(super) fn clear(&mut self) {
        self.bits.iter_mut().for_each(|word| *word = 0);
        self.stack.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indexmap::IndexSet;

    /// Interleave inserts and pops against a real `IndexSet` and check
    /// that the pop sequence is identical: LIFO over insertion order,
    /// no re-push while present.
    #[test]
    fn dirty_list_matches_indexset_pop_order() {
        let mut dirty = DirtyList::new(256);
        let mut reference: IndexSet<SignalId> = IndexSet::new();

        let script: &[&[usize]] = &[
            &[3, 7, 7, 1],
            &[0],
            &[9, 3, 9],
            &[5],
            &[3, 4, 8, 8, 2, 6],
            &[1, 255],
        ];

        let mut pops = 0;
        for step in script {
            for &id in *step {
                let signal_id = SignalId::from_index(id);
                dirty.insert(signal_id);
                reference.insert(signal_id);
            }
            // After each insert batch, pop two (IndexSet::pop and
            // DirtyList::pop must agree, including empty-pops).
            for _ in 0..2 {
                let a = dirty.pop();
                let b = reference.pop();
                pops += 1;
                assert_eq!(a.map(|s| s.index()), b.map(|s| s.index()));
            }
        }
        while !reference.is_empty() {
            let a = dirty.pop();
            let b = reference.pop();
            pops += 1;
            assert_eq!(a.map(|s| s.index()), b.map(|s| s.index()));
        }
        assert!(pops >= script.len() * 2);
        assert!(dirty.pop().is_none());
        assert_eq!(reference.pop(), None);
    }

    #[test]
    fn dirty_list_contains_and_clear() {
        let mut dirty = DirtyList::new(130);
        assert!(!dirty.contains(SignalId::from_index(0)));
        dirty.insert(SignalId::from_index(0));
        dirty.insert(SignalId::from_index(64));
        dirty.insert(SignalId::from_index(129));
        assert!(dirty.contains(SignalId::from_index(0)));
        assert!(dirty.contains(SignalId::from_index(64)));
        assert!(dirty.contains(SignalId::from_index(129)));
        assert!(!dirty.contains(SignalId::from_index(63)));
        assert!(!dirty.contains(SignalId::from_index(65)));

        assert_eq!(dirty.pop().map(|s| s.index()), Some(129));
        assert!(!dirty.contains(SignalId::from_index(129)));
        assert!(dirty.contains(SignalId::from_index(64)));

        dirty.clear();
        assert!(dirty.pop().is_none());
        assert!(!dirty.contains(SignalId::from_index(0)));
        assert!(!dirty.contains(SignalId::from_index(64)));

        // After clear, inserts work again and re-push everything.
        dirty.insert(SignalId::from_index(64));
        assert_eq!(dirty.pop().map(|s| s.index()), Some(64));
    }
}
