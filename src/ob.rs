use ibig::{ubig, UBig};
use murmur3::murmur3_32;
use once_cell::sync::Lazy;
use std::io::Cursor;

fn muk(syd: u32, key: u64) -> u32 {
    let bytes = key.to_le_bytes();
    let lo = bytes.get(0).or(Some(&0)).unwrap();
    let hi = bytes.get(1).or(Some(&0)).unwrap();
    murmur3_32(&mut Cursor::new([*lo, *hi]), syd).unwrap()
}

#[test]
fn test_muk() {
    assert_eq!(muk(0, 0x101), 0x42081a9b);
    assert_eq!(muk(0, 0x201), 0x64c7667e);
    assert_eq!(muk(0, 0x4812), 0xa30782dc);
}

static UX_1_0000: Lazy<UBig> = Lazy::new(|| ubig!(0x10000));
static UX_FFFF_FFFF: Lazy<UBig> = Lazy::new(|| ubig!(0xFFFFFFFF));
static UX_1_0000_0000: Lazy<UBig> = Lazy::new(|| ubig!(0x1_00000000));
static UX_FFFF_FFFF_FFFF_FFFF: Lazy<UBig> = Lazy::new(|| ubig!(0xFFFFFFFF_FFFFFFFF));
static LO_MASK: Lazy<UBig> = Lazy::new(|| ubig!(0x00000000_FFFFFFFF));
static HI_MASK: Lazy<UBig> = Lazy::new(|| ubig!(0xFFFFFFFF_00000000));

#[allow(non_snake_case)]
fn F(j: usize, arg: u64) -> u64 {
    let raku: [u32; 4] = [0xb76d5eed, 0xee281300, 0x85bcae01, 0x4b387af7];
    muk(*raku.get(j).unwrap(), arg) as u64
}

type FType = fn(usize, u64) -> u64;

pub fn fein(arg: &UBig) -> UBig {
    let lo = &*LO_MASK & arg;
    let hi = &*HI_MASK & arg;

    if arg >= &UX_1_0000 && arg <= &UX_FFFF_FFFF {
        let arg_64: u64 = arg.try_into().unwrap();
        UBig::from(0x10000 + feis(arg_64 - 0x10000))
    } else if arg >= &UX_1_0000_0000 && arg <= &UX_FFFF_FFFF_FFFF_FFFF {
        hi | fein(&lo)
    } else {
        arg.clone()
    }
}

pub fn fynd(arg: &UBig) -> UBig {
    let lo = &*LO_MASK & arg;
    let hi = &*HI_MASK & arg;

    if arg >= &UX_1_0000 && arg <= &UX_FFFF_FFFF {
        let arg_64: u64 = arg.try_into().unwrap();
        UBig::from(0x10000 + tail(arg_64 - 0x10000))
    } else if arg >= &UX_1_0000_0000 && arg <= &UX_FFFF_FFFF_FFFF_FFFF {
        hi | fynd(&lo)
    } else {
        arg.clone()
    }
}

fn feis(arg: u64) -> u64 {
    Fe(4, 65535, 65536, 0xFFFF_FFFF, F, arg)
}

#[allow(non_snake_case)]
fn Fe(r: usize, a: u64, b: u64, k: u64, f: FType, m: u64) -> u64 {
    let c = fe(r, a, b, f, m);
    if c < k {
        c
    } else {
        fe(r, a, b, f, c)
    }
}

#[allow(non_snake_case)]
fn fe(r: usize, a: u64, b: u64, f: FType, m: u64) -> u64 {
    fn fe_loop(r: usize, a: u64, b: u64, f: FType, m: u64, j: usize, ell: u64, arr: u64) -> u64 {
        if j > r {
            return if r % 2 != 0 {
                a * arr + ell
            } else if arr == a {
                a * arr + ell
            } else {
                a * ell + arr
            };
        } else {
            let eff = f(j - 1, arr);
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

fn tail(arg: u64) -> u64 {
    Fen(4, 65535, 65536, 0xFFFF_FFFF, F, arg)
}

#[allow(non_snake_case)]
fn Fen(r: usize, a: u64, b: u64, k: u64, f: FType, m: u64) -> u64 {
    let c = fen(r, a, b, f, m);
    if c < k {
        c
    } else {
        fen(r, a, b, f, c)
    }
}

#[allow(non_snake_case)]
fn fen(r: usize, a: u64, b: u64, f: FType, m: u64) -> u64 {
    fn fe_loop(r: usize, a: u64, b: u64, f: FType, m: u64, j: usize, ell: u64, arr: u64) -> u64 {
        if j < 1 {
            a * arr + ell
        } else {
            let eff = f(j - 1, ell);
            let tmp = if j % 2 != 0 {
                (arr + a - eff % a) % a
            } else {
                (arr + b - eff % b) % b
            };
            fe_loop(r, a, b, f, m, j - 1, tmp, ell)
        }
    }

    let ahh = if r % 2 != 0 { m / a } else { m % a };

    let ale = if r % 2 != 0 { m % a } else { m / a };

    let L = if ale == a { ahh } else { ale };

    let R = if ale == a { ale } else { ahh };

    fe_loop(r, a, b, f, m, r, L, R)
}
