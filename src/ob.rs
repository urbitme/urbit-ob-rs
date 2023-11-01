use lazy_static::lazy_static;
use murmur3::murmur3_32;
use num_bigint::BigUint;
use std::io::Cursor;
use std::ops::{BitAnd, Div};

fn muk(syd: u32, _len: u32, key: &BigUint) -> Option<BigUint> {
    let lo = BigUint::from(0x00FF_u16)
        .bitand(key)
        .to_bytes_le()
        .get(0)
        .copied()?;
    let hi = BigUint::from(0xFF00_u16)
        .bitand(key)
        .div(BigUint::from(256_u16))
        .to_bytes_le()
        .get(0)
        .copied()?;
    murmur3_32(&mut Cursor::new([lo, hi]), syd)
        .ok()
        .map(BigUint::from)
}

#[test]
fn test_muk() {
    assert_eq!(
        muk(0, 2, &BigUint::from(0x101_u32)),
        Some(BigUint::from(0x42081a9b_u32))
    );
    assert_eq!(
        muk(0, 2, &BigUint::from(0x201_u32)),
        Some(BigUint::from(0x64c7667e_u32))
    );
    assert_eq!(
        muk(0, 2, &BigUint::from(0x4812_u32)),
        Some(BigUint::from(0xa30782dc_u32))
    );
}

lazy_static! {
    static ref UX_1_0000: BigUint = BigUint::from(0x10000_u32);
    static ref UX_FFFF_FFFF: BigUint = BigUint::from(0xFFFFFFFF_u32);
    static ref UX_1_0000_0000: BigUint = BigUint::from(0x1_00000000_u64);
    static ref UX_FFFF_FFFF_FFFF_FFFF: BigUint = BigUint::from(0xFFFFFFFF_FFFFFFFF_u64);
    static ref U_65535: BigUint = BigUint::from(65535_u16);
    static ref U_65536: BigUint = BigUint::from(65536_u32);
}

#[allow(non_snake_case)]
pub fn F(j: usize, arg: &BigUint) -> Option<BigUint> {
    let raku: [u32; 4] = [0xb76d5eed, 0xee281300, 0x85bcae01, 0x4b387af7];
    muk(*raku.get(j)?, 2, arg)
}

type FType = fn(usize, &BigUint) -> Option<BigUint>;

pub fn fein(arg: &BigUint) -> BigUint {
    let lo = BigUint::from(0x00000000_FFFFFFFF_u64) & arg;
    let hi = BigUint::from(0xFFFFFFFF_00000000_u64) & arg;

    if arg >= &UX_1_0000 && arg <= &UX_FFFF_FFFF {
        UX_1_0000.clone() + feis(&(arg.clone() - UX_1_0000.clone()))
    } else if arg >= &UX_1_0000_0000 && arg <= &UX_FFFF_FFFF_FFFF_FFFF {
        hi | fein(&lo)
    } else {
        arg.clone()
    }
}

pub fn fynd(arg: &BigUint) -> BigUint {
    let lo = BigUint::from(0x00000000_FFFFFFFF_u64) & arg;
    let hi = BigUint::from(0xFFFFFFFF_00000000_u64) & arg;

    if arg >= &UX_1_0000 && arg <= &UX_FFFF_FFFF {
        UX_1_0000.clone() + tail(&(arg.clone() - UX_1_0000.clone()))
    } else if arg >= &UX_1_0000_0000 && arg <= &UX_FFFF_FFFF_FFFF_FFFF {
        hi | fynd(&lo)
    } else {
        arg.clone()
    }
}

pub fn feis(arg: &BigUint) -> BigUint {
    Fe(4, &U_65535, &U_65536, &UX_FFFF_FFFF, F, arg)
}

#[allow(non_snake_case)]
pub fn Fe(r: usize, a: &BigUint, b: &BigUint, k: &BigUint, f: FType, m: &BigUint) -> BigUint {
    let c = fe(r, a, b, f, m);
    if c < *k {
        c
    } else {
        fe(r, a, b, f, &c)
    }
}

#[allow(non_snake_case)]
pub fn fe(r: usize, a: &BigUint, b: &BigUint, f: FType, m: &BigUint) -> BigUint {
    fn fe_loop(r: usize, a: &BigUint, b: &BigUint, f: FType, m: &BigUint, j: usize, ell: BigUint, arr: BigUint) -> BigUint {
        if j > r {
            return if r % 2 != 0 {
                a * arr + ell
            } else if arr == *a {
                a * arr + ell
            } else {
                a * ell + arr
            }
        } else {
            let eff = f(j - 1, &arr).unwrap();
            let tmp = if j % 2 != 0 {
                (ell + eff) % a
            } else {
                (ell + eff) % b
            };
            fe_loop(r, a, b, f, m, j + 1, arr, tmp)
        }
    }

    let L = m % a;
    let R = m / a;

    fe_loop(r, a, b, f, m, 1, L, R)
}

pub fn tail(arg: &BigUint) -> BigUint {
    Fen(4, &U_65535, &U_65536, &UX_FFFF_FFFF, F, arg)
}

#[allow(non_snake_case)]
pub fn Fen(r: usize, a: &BigUint, b: &BigUint, k: &BigUint, f: FType, m: &BigUint) -> BigUint {
    let c = fen(r, a, b, f, m);
    if c < *k {
        c
    } else {
        fen(r, a, b, f, &c)
    }
}

#[allow(non_snake_case)]
pub fn fen(r: usize, a: &BigUint, b: &BigUint, f: FType, m: &BigUint) -> BigUint {
    fn fe_loop(r: usize, a: &BigUint, b: &BigUint, f: FType, m: &BigUint, j: usize, ell: &BigUint, arr: &BigUint) -> BigUint {
        if j < 1 {
            a * arr + ell
        } else {
            let eff = f(j - 1, &ell).unwrap();
            let tmp = if j % 2 != 0 {
                (arr + a - eff % a) % a
            } else {
                (arr + b - eff % b) % b
            };
            fe_loop(r, a, b, f, m, j - 1, &tmp, ell)
        }
    }

    let ahh = if r % 2 != 0 {
        m / a
    } else {
        m % a
    };

    let ale = if r % 2 != 0 {
        m % a
    } else {
        m / a
    };

    let L = if ale == *a {
        ahh.clone()
    } else {
        ale.clone()
    };

    let R = if ale == *a {
        ale
    } else {
        ahh
    };

    fe_loop(r, a, b, f, m, r, &L, &R)
}

