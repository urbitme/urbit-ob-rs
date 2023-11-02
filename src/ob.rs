use murmur3::murmur3_32;
use once_cell::sync::Lazy;
use ibig::{UBig, ubig};
use std::io::Cursor;
use std::ops::{BitAnd, Div};

fn muk(syd: u32, _len: u32, key: &UBig) -> Option<UBig> {
    let lo = ubig!(0x00FF)
        .bitand(key)
        .to_le_bytes()
        .get(0).or(Some(&0))
        .copied()?;
    let hi = ubig!(0xFF00)
        .bitand(key)
        .div(ubig!(256))
        .to_le_bytes()
        .get(0).or(Some(&0))
        .copied()?;
    murmur3_32(&mut Cursor::new([lo, hi]), syd)
        .ok()
        .map(UBig::from)
}

#[test]
fn test_muk() {
    assert_eq!(
        muk(0, 2, &ubig!(0x101)),
        Some(ubig!(0x42081a9b))
    );
    assert_eq!(
        muk(0, 2, &ubig!(0x201)),
        Some(ubig!(0x64c7667e))
    );
    assert_eq!(
        muk(0, 2, &ubig!(0x4812)),
        Some(ubig!(0xa30782dc))
    );
}

static UX_FFFF_FFFF: Lazy<UBig> = Lazy::new(|| { ubig!(0xFFFFFFFF) });
static UX_1_0000_0000: Lazy<UBig> = Lazy::new(|| { ubig!(0x1_00000000) });
static UX_FFFF_FFFF_FFFF_FFFF: Lazy<UBig> = Lazy::new(|| { ubig!(0xFFFFFFFF_FFFFFFFF) });
static U_65535: Lazy<UBig> = Lazy::new(|| { ubig!(65535) });
static U_65536: Lazy<UBig> = Lazy::new(|| { ubig!(65536) });

#[allow(non_snake_case)]
fn F(j: usize, arg: &UBig) -> Option<UBig> {
    let raku: [u32; 4] = [0xb76d5eed, 0xee281300, 0x85bcae01, 0x4b387af7];
    muk(*raku.get(j)?, 2, arg)
}

type FType = fn(usize, &UBig) -> Option<UBig>;

pub fn fein(arg: &UBig) -> UBig {
    let ux_1_0000 = ubig!(0x10000);
    let lo = ubig!(0x00000000_FFFFFFFF) & arg;
    let hi = ubig!(0xFFFFFFFF_00000000) & arg;

    if arg >= &ux_1_0000 && arg <= &UX_FFFF_FFFF {
        ux_1_0000.clone() + feis(&(arg - &ux_1_0000))
    } else if arg >= &UX_1_0000_0000 && arg <= &UX_FFFF_FFFF_FFFF_FFFF {
        hi | fein(&lo)
    } else {
        arg.clone()
    }
}

pub fn fynd(arg: &UBig) -> UBig {
    let ux_1_0000 = ubig!(0x10000);
    let lo = ubig!(0x00000000_FFFFFFFF) & arg;
    let hi = ubig!(0xFFFFFFFF_00000000) & arg;

    if arg >= &ux_1_0000 && arg <= &UX_FFFF_FFFF {
        &ux_1_0000 + tail(&(arg.clone() - &ux_1_0000))
    } else if arg >= &UX_1_0000_0000 && arg <= &UX_FFFF_FFFF_FFFF_FFFF {
        hi | fynd(&lo)
    } else {
        arg.clone()
    }
}

fn feis(arg: &UBig) -> UBig {
    Fe(4, &U_65535, &U_65536, &UX_FFFF_FFFF, F, arg)
}

#[allow(non_snake_case)]
fn Fe(r: usize, a: &UBig, b: &UBig, k: &UBig, f: FType, m: &UBig) -> UBig {
    let c = fe(r, a, b, f, m);
    if c < *k {
        c
    } else {
        fe(r, a, b, f, &c)
    }
}

#[allow(non_snake_case)]
fn fe(r: usize, a: &UBig, b: &UBig, f: FType, m: &UBig) -> UBig {
    fn fe_loop(r: usize, a: &UBig, b: &UBig, f: FType, m: &UBig, j: usize, ell: UBig, arr: UBig) -> UBig {
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

fn tail(arg: &UBig) -> UBig {
    Fen(4, &U_65535, &U_65536, &UX_FFFF_FFFF, F, arg)
}

#[allow(non_snake_case)]
fn Fen(r: usize, a: &UBig, b: &UBig, k: &UBig, f: FType, m: &UBig) -> UBig {
    let c = fen(r, a, b, f, m);
    if c < *k {
        c
    } else {
        fen(r, a, b, f, &c)
    }
}

#[allow(non_snake_case)]
fn fen(r: usize, a: &UBig, b: &UBig, f: FType, m: &UBig) -> UBig {
    fn fe_loop(r: usize, a: &UBig, b: &UBig, f: FType, m: &UBig, j: usize, ell: &UBig, arr: &UBig) -> UBig {
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

