//! Integer math helpers for hardware sizing: `pow`, `clog2` (ceiling of
//! log base 2), and `is_pow2`, used by type inference and width
//! calculations.

pub fn pow(n: u64, k: u64) -> u64 {
    let mut p = 1;
    for _ in 0..k {
        p *= n
    }
    p
}

pub fn clog2(n: u64) -> u64 {
    let mut result = 0;
    while n > (1 << result) {
        result += 1;
    }
    result
}

pub fn is_pow2(n: u64) -> bool {
    n != 0 && (n & (n - 1)) == 0
}
