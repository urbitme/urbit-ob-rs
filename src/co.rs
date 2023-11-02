use hex;
use once_cell::sync::Lazy;
use num_bigint::BigUint;
use num_traits::{Pow, Zero};
use std::{cmp::PartialEq, collections::HashMap, ops::Rem};

use super::ob::{fein, fynd};

/// Parsing and Formatting errors
#[derive(thiserror::Error, Debug, Clone)]
pub enum Error {
    #[error("Value must begin with leader: {0}")]
    LeaderMissing(String),
    #[error("Invalid prefix value: {0}")]
    InvalidPrefix(String),
    #[error("Invalid suffix value: {0}")]
    InvalidSuffix(String),
    #[error("Invalid section: {0}")]
    InvalidSection(String),
    #[error("Full six-letter words required")]
    ZeroPadRequired,
}

/// Ordered list of all prefixes.  Use to lookup a prefix by numerical value.
pub const PREFIXES: [&'static str; 256] = [
    "doz", "mar", "bin", "wan", "sam", "lit", "sig", "hid", "fid", "lis", "sog", "dir", "wac",
    "sab", "wis", "sib", "rig", "sol", "dop", "mod", "fog", "lid", "hop", "dar", "dor", "lor",
    "hod", "fol", "rin", "tog", "sil", "mir", "hol", "pas", "lac", "rov", "liv", "dal", "sat",
    "lib", "tab", "han", "tic", "pid", "tor", "bol", "fos", "dot", "los", "dil", "for", "pil",
    "ram", "tir", "win", "tad", "bic", "dif", "roc", "wid", "bis", "das", "mid", "lop", "ril",
    "nar", "dap", "mol", "san", "loc", "nov", "sit", "nid", "tip", "sic", "rop", "wit", "nat",
    "pan", "min", "rit", "pod", "mot", "tam", "tol", "sav", "pos", "nap", "nop", "som", "fin",
    "fon", "ban", "mor", "wor", "sip", "ron", "nor", "bot", "wic", "soc", "wat", "dol", "mag",
    "pic", "dav", "bid", "bal", "tim", "tas", "mal", "lig", "siv", "tag", "pad", "sal", "div",
    "dac", "tan", "sid", "fab", "tar", "mon", "ran", "nis", "wol", "mis", "pal", "las", "dis",
    "map", "rab", "tob", "rol", "lat", "lon", "nod", "nav", "fig", "nom", "nib", "pag", "sop",
    "ral", "bil", "had", "doc", "rid", "moc", "pac", "rav", "rip", "fal", "tod", "til", "tin",
    "hap", "mic", "fan", "pat", "tac", "lab", "mog", "sim", "son", "pin", "lom", "ric", "tap",
    "fir", "has", "bos", "bat", "poc", "hac", "tid", "hav", "sap", "lin", "dib", "hos", "dab",
    "bit", "bar", "rac", "par", "lod", "dos", "bor", "toc", "hil", "mac", "tom", "dig", "fil",
    "fas", "mit", "hob", "har", "mig", "hin", "rad", "mas", "hal", "rag", "lag", "fad", "top",
    "mop", "hab", "nil", "nos", "mil", "fop", "fam", "dat", "nol", "din", "hat", "nac", "ris",
    "fot", "rib", "hoc", "nim", "lar", "fit", "wal", "rap", "sar", "nal", "mos", "lan", "don",
    "dan", "lad", "dov", "riv", "bac", "pol", "lap", "tal", "pit", "nam", "bon", "ros", "ton",
    "fod", "pon", "sov", "noc", "sor", "lav", "mat", "mip", "fip",
];

/// Ordered list of all suffixes.  Use to lookup a suffix by numerical value.
pub const SUFFIXES: [&'static str; 256] = [
    "zod", "nec", "bud", "wes", "sev", "per", "sut", "let", "ful", "pen", "syt", "dur", "wep",
    "ser", "wyl", "sun", "ryp", "syx", "dyr", "nup", "heb", "peg", "lup", "dep", "dys", "put",
    "lug", "hec", "ryt", "tyv", "syd", "nex", "lun", "mep", "lut", "sep", "pes", "del", "sul",
    "ped", "tem", "led", "tul", "met", "wen", "byn", "hex", "feb", "pyl", "dul", "het", "mev",
    "rut", "tyl", "wyd", "tep", "bes", "dex", "sef", "wyc", "bur", "der", "nep", "pur", "rys",
    "reb", "den", "nut", "sub", "pet", "rul", "syn", "reg", "tyd", "sup", "sem", "wyn", "rec",
    "meg", "net", "sec", "mul", "nym", "tev", "web", "sum", "mut", "nyx", "rex", "teb", "fus",
    "hep", "ben", "mus", "wyx", "sym", "sel", "ruc", "dec", "wex", "syr", "wet", "dyl", "myn",
    "mes", "det", "bet", "bel", "tux", "tug", "myr", "pel", "syp", "ter", "meb", "set", "dut",
    "deg", "tex", "sur", "fel", "tud", "nux", "rux", "ren", "wyt", "nub", "med", "lyt", "dus",
    "neb", "rum", "tyn", "seg", "lyx", "pun", "res", "red", "fun", "rev", "ref", "mec", "ted",
    "rus", "bex", "leb", "dux", "ryn", "num", "pyx", "ryg", "ryx", "fep", "tyr", "tus", "tyc",
    "leg", "nem", "fer", "mer", "ten", "lus", "nus", "syl", "tec", "mex", "pub", "rym", "tuc",
    "fyl", "lep", "deb", "ber", "mug", "hut", "tun", "byl", "sud", "pem", "dev", "lur", "def",
    "bus", "bep", "run", "mel", "pex", "dyt", "byt", "typ", "lev", "myl", "wed", "duc", "fur",
    "fex", "nul", "luc", "len", "ner", "lex", "rup", "ned", "lec", "ryd", "lyd", "fen", "wel",
    "nyd", "hus", "rel", "rud", "nes", "hes", "fet", "des", "ret", "dun", "ler", "nyr", "seb",
    "hul", "ryl", "lud", "rem", "lys", "fyn", "wer", "ryc", "sug", "nys", "nyl", "lyn", "dyn",
    "dem", "lux", "fed", "sed", "bec", "mun", "lyr", "tes", "mud", "nyt", "byr", "sen", "weg",
    "fyr", "mur", "tel", "rep", "teg", "pec", "nel", "nev", "fes",
];

/// Map from prefix name to integer value.  Use to validate a prefix or lookup the numeric value.
pub static PREFIX_VALUES: Lazy<HashMap<&'static str, u8>> = Lazy::new(|| {
    let mut h = HashMap::with_capacity(256);
    for (index, value) in PREFIXES.iter().enumerate() {
        h.insert(*value, index as u8);
    }
    h
});

/// Map from suffix name to integer value.  Use to validate a suffix or lookup the numeric value.
pub static SUFFIX_VALUES: Lazy<HashMap<&'static str, u8>> = Lazy::new(|| {
    let mut h = HashMap::with_capacity(256);
    for (index, value) in SUFFIXES.iter().enumerate() {
        h.insert(*value, index as u8);
    }
    h
});

fn met(a: usize, b: &BigUint) -> usize {
    let zero = BigUint::zero();
    let mut b = b.clone();
    let mut c = 0;
    loop {
        if b.eq(&zero) {
            return c
        }
        b = b >> 2.pow(a);
        c = c + 1;
    }
}

fn end(a: usize, c: &BigUint) -> BigUint {
    c.rem(2u64.pow(2.pow(a) as u32) as u64)
}

/// Convert a number (BigUint) to a @q-encoded string.
pub fn patq(n: &BigUint) -> String {
    let buf = n.to_bytes_be();
    buf2patq(&buf)
}

/// Convert a Buffer into a @q-encoded string.
pub fn buf2patq(buf: &[u8]) -> String {
    buf2pat(buf, ".~", false, false)
}

/// Convert a Buffer into a @p-encoded string.
pub fn buf2patp(buf: &[u8]) -> String {
    buf2pat(buf, "~", true, true)
}

/// Generalized formatter for @p and @q encoded strings.
pub fn buf2pat(buf: &[u8], leader: &str, zero_pad: bool, quad_sep: bool) -> String {
    // Galaxies are never zero-padded
    let zero_pad = buf.len() != 1 && zero_pad;

    // Zero pad the buffer so it is always even length
    let mut v: Vec<u8>;
    let bytes: &[u8] = if buf.len() % 2 != 0 {
        v = Vec::with_capacity(buf.len() + 1);
        v.push(0);
        v.extend_from_slice(buf);
        &v
    } else {
        buf
    };

    let mut timp: usize = bytes.len() / 2;
    let mut name = String::new();
    for chunk in bytes.chunks(2) {
        match chunk {
            &[pre, suf] => {
                if name.is_empty() {
                    if zero_pad || pre != 0 {
                        name.push_str(PREFIXES[pre as usize]);
                    }
                } else {
                    if quad_sep && timp % 4 == 0 {
                        name.push_str("--");
                    } else {
                        name.push_str("-");
                    }
                    name.push_str(PREFIXES[pre as usize]);
                }
                name.push_str(SUFFIXES[suf as usize]);
            }
            _ => panic!("buf2pat bug!"),
        }
        timp = timp - 1;
    }
    format!("{}{}", leader, name)
}

/// Convert a hex-encoded string to a @q-encoded string.
pub fn hex2patq(hex: &str) -> String {
    let hex = if hex.len() % 2 != 0 {
        format!("0{hex}")
    } else {
        hex.to_string()
    };

    let buf = hex::decode(hex).unwrap();
    buf2patq(&buf)
}

/// Convert a @q-encoded string to a hex-encoded string.
pub fn patq2hex(name: &str) -> Result<String, Error> {
    patq2bn(name).map(|bn| bn.to_str_radix(16)).map(|hex| {
        if hex.len() % 2 != 0 {
            format!("0{hex}")
        } else {
            hex
        }
    })
}

/// Convert a @q-encoded string to a BigUint
pub fn patq2bn(name: &str) -> Result<BigUint, Error> {
    let syls = patq2syls(name)?;
    let buf = syls2buffer(&syls);

    Ok(BigUint::from_bytes_be(&buf))
}

/// Convert a @q-encoded string to a decimal-encoded string.
pub fn patq2dec(name: &str) -> Result<String, Error> {
    patq2bn(name).map(|bn| bn.to_str_radix(10))
}

/// Convert a hex-encoded string to a @p-encoded string.
pub fn hex2patp(hex: &str) -> String {
    let bn = BigUint::parse_bytes(hex.as_bytes(), 16).unwrap();
    patp(&bn)
}

/// Convert a @p-encoded string to a BigUint
pub fn patp2bn(name: &str) -> Result<BigUint, Error> {
    let syls = patp2syls(name)?;
    let buf = syls2buffer(&syls);

    Ok(fynd(&BigUint::from_bytes_be(&buf)))
}

/// Convert a @p-encoded string to a hex-encoded string.
pub fn patp2hex(name: &str) -> Result<String, Error> {
    patp2bn(name).map(|bn| bn.to_str_radix(16))
}

/// Convert a @p-encoded string to a decimal-encoded string.
pub fn patp2dec(name: &str) -> Result<String, Error> {
    patp2bn(name).map(|bn| bn.to_str_radix(10))
}

/// Convert a number (BigUint) to a @p-encoded string.
pub fn patp(n: &BigUint) -> String {
    let buf = fein(n).to_bytes_be();
    buf2patp(&buf)
}

/// Determine the ship class of a @p value.
pub fn clan(who: &str) -> &'static str {
    let n = patp2bn(who).unwrap();
    let wid = met(3, &n);

    if wid <= 1 {
        "galaxy"
    } else if wid == 2 {
        "star"
    } else if wid <= 4 {
        "planet"
    } else if wid <= 8 {
        "moon"
    } else {
        "comet"
    }
}

/// Determine the parent of a @p value.
pub fn sein(name: &str) -> Result<String, Error> {
    let who = patp2bn(name)?;
    let mir = clan(name);

    let res = match mir {
        "galaxy" => who,
        "star" => end(3, &who),
        "planet" => end(4, &who),
        "moon" => end(5, &who),
        _ => BigUint::zero(),
    };

    Ok(patp(&res))
}

/// Generalized parsing for @p and @q values.
pub fn pat2syls<'a, 'b>(
    pat: &'a str,
    leader: &'b str,
    force_zero_pad: bool,
) -> Result<Vec<&'a str>, Error> {
    if let Some(rest) = pat.strip_prefix(leader) {
        let words: Vec<&str> = rest.split('-').filter(|s| !s.is_empty()).collect();
        if words.len() == 1 && words[0].len() == 3 {
            if SUFFIX_VALUES.contains_key(words[0]) {
                Ok(words)
            } else {
                Err(Error::InvalidSuffix(words[0].to_string()))
            }
        } else {
            let mut syls = Vec::with_capacity(words.len() * 2);
            let mut first_word = true;
            for word in words.iter() {
                if word.len() == 6 {
                    let (pre, suf) = word.split_at(3);
                    if PREFIX_VALUES.contains_key(pre) {
                        syls.push(pre);
                    } else {
                        return Err(Error::InvalidPrefix(pre.to_string()));
                    }
                    if SUFFIX_VALUES.contains_key(suf) {
                        syls.push(suf);
                    } else {
                        return Err(Error::InvalidSuffix(suf.to_string()));
                    }
                } else if word.len() == 3 && first_word {
                    if !force_zero_pad {
                        if SUFFIX_VALUES.contains_key(word) {
                            syls.push(word);
                        } else {
                            return Err(Error::InvalidSuffix(word.to_string()));
                        }
                    } else {
                        return Err(Error::ZeroPadRequired);
                    }
                } else {
                    return Err(Error::InvalidSection(word.to_string()));
                }
                first_word = false;
            }
            Ok(syls)
        }
    } else {
        Err(Error::LeaderMissing(leader.to_string()))
    }
}

/// Parse a @p-encoded value into a vector of individual syllables
pub fn patp2syls(pat: &str) -> Result<Vec<&str>, Error> {
    pat2syls(pat, "~", true)
}

/// Parse a @q-encoded value into a vector of individual syllables
pub fn patq2syls(pat: &str) -> Result<Vec<&str>, Error> {
    pat2syls(pat, ".~", false)
}

/// Convert syllables into a buffer of their integer values.
pub fn syls2buffer(syls: &[&str]) -> Vec<u8> {
    // Start with suffix when odd number of syllables
    let mut suffix: bool = syls.len() % 2 == 1;
    let mut buf = Vec::with_capacity(syls.len());
    for syl in syls.iter() {
        if suffix {
            buf.push(SUFFIX_VALUES.get(syl).unwrap().clone());
        } else {
            buf.push(PREFIX_VALUES.get(syl).unwrap().clone());
        }
        suffix = !suffix;
    }
    buf
}

/// Flexible and customizable validation for pat type values.
pub fn is_valid_pat(name: &str) -> bool {
    pat2syls(name, "", false).is_ok()
}

/// Validate a @p-encoded string.
pub fn is_valid_patp(name: &str) -> bool {
    pat2syls(name, "~", true).is_ok()
}

/// Validate a @q-encoded string.
pub fn is_valid_patq(name: &str) -> bool {
    pat2syls(name, ".~", false).is_ok()
}

/// Equality test for a @p and @q.
pub fn eq_patq(p: &str, q: &str) -> Result<bool, Error> {
    let p_val = patp2bn(p)?;
    let q_val = patq2bn(q)?;
    Ok(p_val == q_val)
}
