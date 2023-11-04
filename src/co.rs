use ibig::UBig;
use num_traits::{Pow, Zero};
use once_cell::sync::Lazy;
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
    #[error("Invalid hex string: {0}")]
    InvalidHex(String),
    #[error("Invalid decimal string: {0}")]
    InvalidDecimal(String),
    #[error("Invalid separator")]
    InvalidSeparator,
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

/// Validate the input as a prefix, and return a static copy
pub fn static_pre(pre: &str) -> Result<&'static str, Error> {
    PREFIX_VALUES
        .get_key_value(pre)
        .ok_or(Error::InvalidPrefix(pre.to_string()))
        .map(|(k, _)| *k)
}

/// Validate the input as a suffix, and return a static copy
pub fn static_suf(suf: &str) -> Result<&'static str, Error> {
    SUFFIX_VALUES
        .get_key_value(suf)
        .ok_or(Error::InvalidSuffix(suf.to_string()))
        .map(|(k, _)| *k)
}

/// Get a prefix value with validation
pub fn prefix_value(pre: &str) -> Result<u8, Error> {
    PREFIX_VALUES
        .get(pre)
        .ok_or(Error::InvalidPrefix(pre.to_string()))
        .map(|v| *v)
}

/// Get a suffix value with validation
pub fn suffix_value(suf: &str) -> Result<u8, Error> {
    SUFFIX_VALUES
        .get(suf)
        .ok_or(Error::InvalidSuffix(suf.to_string()))
        .map(|v| *v)
}

fn met(a: usize, b: &UBig) -> usize {
    let zero = UBig::zero();
    let mut b = b.clone();
    let mut c = 0;
    loop {
        if b.eq(&zero) {
            return c;
        }
        b = b >> 2.pow(a) as usize;
        c = c + 1;
    }
}

fn end(a: usize, c: &UBig) -> UBig {
    c.rem(UBig::from(2u64.pow(2u32.pow(a as u32))))
}

/// Convert an unsigned integer into a @q-encoded string.
pub fn patq<T>(n: T) -> String
where
    UBig: From<T>,
{
    big2patq(&UBig::from(n))
}

/// Convert a bignum (UBig) into a @q-encoded string.
pub fn big2patq(n: &UBig) -> String {
    let buf = if n.is_zero() {
        vec![0]
    } else {
        n.to_be_bytes()
    };
    buf2patq(&buf)
}

/// Convert an unsigned integer into a @p-encoded string.
pub fn patp<T>(n: T) -> String
where
    UBig: From<T>,
{
    big2patp(&UBig::from(n))
}

/// Convert a bignum (UBig) to a @p-encoded string.
pub fn big2patp(n: &UBig) -> String {
    let n = fein(n);
    let buf = if n.is_zero() {
        vec![0]
    } else {
        n.to_be_bytes()
    };
    buf2patp(&buf)
}

/// Convert a Buffer into a @q-encoded string.
pub fn buf2patq(buf: &[u8]) -> String {
    buf2pat(buf, ".~", false, false)
}

/// Convert a Buffer into a @p-encoded string.
pub fn buf2patp(buf: &[u8]) -> String {
    buf2pat(buf, "~", true, true)
}

/// General formatter for @p and @q encoded strings.
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
pub fn hex2patq(hex: &str) -> Result<String, Error> {
    UBig::from_str_radix(hex, 16)
        .map(|n| big2patq(&n))
        .map_err(|_| Error::InvalidHex(hex.to_string()))
}

/// Convert a hex-encoded string to a @p-encoded string.
pub fn hex2patp(hex: &str) -> Result<String, Error> {
    UBig::from_str_radix(hex, 16)
        .map(|n| big2patp(&n))
        .map_err(|_| Error::InvalidHex(hex.to_string()))
}

/// Convert a decimal encoded string to a @q-encoded string.
pub fn dec2patq(dec: &str) -> Result<String, Error> {
    UBig::from_str_radix(dec, 10)
        .map(|n| big2patq(&n))
        .map_err(|_| Error::InvalidDecimal(dec.to_string()))
}

/// Convert a decimal encoded string to a @q-encoded string.
pub fn dec2patp(dec: &str) -> Result<String, Error> {
    UBig::from_str_radix(dec, 10)
        .map(|n| big2patp(&n))
        .map_err(|_| Error::InvalidDecimal(dec.to_string()))
}

/// Convert a @q-encoded string to a UBig
pub fn patq2big(name: &str) -> Result<UBig, Error> {
    let syls = patq2syls(name)?;
    let buf = syls2buffer(&syls)?;

    Ok(UBig::from_be_bytes(&buf))
}

/// Convert a @p-encoded string to a UBig
pub fn patp2big(name: &str) -> Result<UBig, Error> {
    let syls = patp2syls(name)?;
    let buf = syls2buffer(&syls)?;

    Ok(fynd(&UBig::from_be_bytes(&buf)))
}

/// Parse a @p-encoded value into a vector of individual syllables
pub fn patp2syls(pat: &str) -> Result<Vec<&str>, Error> {
    pat2syls(pat, "~", true, true)
}

/// Parse a @q-encoded value into a vector of individual syllables
pub fn patq2syls(pat: &str) -> Result<Vec<&str>, Error> {
    pat2syls(pat, ".~", false, false)
}

/// General parser for @p and @q values.
pub fn pat2syls(
    pat: &str,
    leader: &str,
    zero_pad: bool,
    quad_sep: bool,
) -> Result<Vec<&'static str>, Error> {
    if let Some(rest) = pat.strip_prefix(leader) {
        let words: Vec<&str> = rest.split('-').collect();
        if words.len() == 1 && words[0].len() == 3 {
            // Galaxy special case
            static_suf(words[0]).map(|s| vec![s])
        } else {
            // An empty word indicates double hyphen
            let mut empties = 0;
            for (i, word) in words.iter().rev().enumerate() {
                if quad_sep && i % 5 == 4 {
                    if !word.is_empty() {
                        return Err(Error::InvalidSeparator);
                    } else {
                        empties += 1;
                    }
                } else if word.is_empty() {
                    return Err(Error::InvalidSeparator);
                }
            }

            // Iterate non-empty words and split into syls
            let mut syls = Vec::with_capacity((words.len() - empties) * 2);
            let mut first_word = true;
            for word in words.iter().filter(|s| !s.is_empty()) {
                if word.len() == 6 {
                    let (pre, suf) = word.split_at(3);
                    syls.push(static_pre(pre)?);
                    syls.push(static_suf(suf)?);
                } else if word.len() == 3 && first_word {
                    if !zero_pad {
                        syls.push(static_suf(word)?);
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

/// Convert syllables into a buffer of their integer values.
pub fn syls2buffer(syls: &[&str]) -> Result<Vec<u8>, Error> {
    // Start with suffix when odd number of syllables
    let mut suffix: bool = syls.len() % 2 == 1;
    let mut buf = Vec::with_capacity(syls.len());
    for syl in syls.iter() {
        if suffix {
            buf.push(suffix_value(syl)?);
        } else {
            buf.push(prefix_value(syl)?);
        }
        suffix = !suffix;
    }
    Ok(buf)
}

/// Convert a @q-encoded string to a hex-encoded string.
pub fn patq2hex(name: &str) -> Result<String, Error> {
    patq2big(name).map(big2hex)
}

/// Convert a @p-encoded string to a hex-encoded string.
pub fn patp2hex(name: &str) -> Result<String, Error> {
    patp2big(name).map(big2hex)
}

// Ensures zero-padded even length hex strings
fn big2hex(n: UBig) -> String {
    let hex = n.in_radix(16).to_string();
    if hex.len() % 2 != 0 {
        format!("0{hex}")
    } else {
        hex
    }
}

/// Convert a @q-encoded string to a decimal-encoded string.
pub fn patq2dec(name: &str) -> Result<String, Error> {
    patq2big(name).map(|n| n.in_radix(10).to_string())
}

/// Convert a @p-encoded string to a decimal-encoded string.
pub fn patp2dec(name: &str) -> Result<String, Error> {
    patp2big(name).map(|n| n.in_radix(10).to_string())
}

/// Determine the ship class of a @p value.
pub fn clan(who: &str) -> Result<&'static str, Error> {
    let n = patp2big(who)?;
    let wid = met(3, &n);

    Ok(if wid <= 1 {
        "galaxy"
    } else if wid == 2 {
        "star"
    } else if wid <= 4 {
        "planet"
    } else if wid <= 8 {
        "moon"
    } else {
        "comet"
    })
}

/// Determine the parent of a @p value.
pub fn sein(name: &str) -> Result<String, Error> {
    let who = patp2big(name)?;
    let mir = clan(name)?;

    let res = match mir {
        "galaxy" => who,
        "star" => end(3, &who),
        "planet" => end(4, &who),
        "moon" => end(5, &who),
        _ => UBig::zero(),
    };

    Ok(big2patp(&res))
}

/// General validation for pat type values.
pub fn is_valid_pat(name: &str, leader: &str, force_zero_pad: bool, quad_sep: bool) -> bool {
    pat2syls(name, leader, force_zero_pad, quad_sep).is_ok()
}

/// Validate a @p-encoded string.
pub fn is_valid_patp(name: &str) -> bool {
    patp2syls(name).is_ok()
}

/// Validate a @q-encoded string.
pub fn is_valid_patq(name: &str) -> bool {
    patq2syls(name).is_ok()
}

/// Equality test for a @p and @q.
pub fn eq_patq(p: &str, q: &str) -> Result<bool, Error> {
    let p_val = patp2big(p)?;
    let q_val = patq2big(q)?;
    Ok(p_val == q_val)
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_patq() {
        assert_eq!(patq(0_u32), ".~zod");
        assert_eq!(patq(0xac_u32), ".~ber");
        assert_eq!(patq(0x23ec_u32), ".~rovfed");
        assert_eq!(patq(0x94cf670c_u32), ".~mocwel-magwep");
        assert_eq!(patq(0xe57f1c2d_u32), ".~sarmed-rinbyn");
        assert_eq!(
            patq(0xed816cd86003df88_u64),
            ".~rivdus-timret-ronwes-hocres"
        );
        assert_eq!(patq(0xd86003df88_u64), ".~ret-ronwes-hocres");
        assert_eq!(
            patq(0x85613e0edad57f5fddc48d3f5808a628_u128),
            ".~rolruc-midwyl-hathes-palsym-fotnul-pagpur-nopful-lomtem"
        );
    }

    #[test]
    fn test_patp() {
        assert_eq!(patp(0_u32), "~zod");
        assert_eq!(patp(0xac_u32), "~ber");
        assert_eq!(patp(0x23ec_u32), "~rovfed");
        assert_eq!(patp(0x94cf670c_u32), "~lapsen-hapmeb");
        assert_eq!(patp(0xe57f1c2d_u32), "~hobpur-habnev");
        assert_eq!(patp(0xed816cd86003df88_u64), "~rivdus-timret-tardet-paslux");
        assert_eq!(patp(0xd86003df88_u64), "~dozret-tardet-paslux");
        assert_eq!(
            patp(0x85613e0edad57f5fddc48d3f5808a628_u128),
            "~rolruc-midwyl-hathes-palsym--fotnul-pagpur-nopful-lomtem"
        );
    }

    #[test]
    fn test_hex2patq() {
        assert_eq!(hex2patq("0").unwrap(), ".~zod");
        assert_eq!(hex2patq("ac").unwrap(), ".~ber");
        assert_eq!(hex2patq("23ec").unwrap(), ".~rovfed");
        assert_eq!(hex2patq("94cf670c").unwrap(), ".~mocwel-magwep");
        assert_eq!(hex2patq("e57f1c2d").unwrap(), ".~sarmed-rinbyn");
        assert_eq!(
            hex2patq("ed816cd86003df88").unwrap(),
            ".~rivdus-timret-ronwes-hocres"
        );
        assert_eq!(hex2patq("d86003df88").unwrap(), ".~ret-ronwes-hocres");
        assert_eq!(
            hex2patq("85613e0edad57f5fddc48d3f5808a628").unwrap(),
            ".~rolruc-midwyl-hathes-palsym-fotnul-pagpur-nopful-lomtem"
        );
    }

    #[test]
    fn test_hex2patp() {
        assert_eq!(hex2patp("0").unwrap(), "~zod");
        assert_eq!(hex2patp("ac").unwrap(), "~ber");
        assert_eq!(hex2patp("23ec").unwrap(), "~rovfed");
        assert_eq!(hex2patp("94cf670c").unwrap(), "~lapsen-hapmeb");
        assert_eq!(hex2patp("e57f1c2d").unwrap(), "~hobpur-habnev");
        assert_eq!(
            hex2patp("ed816cd86003df88").unwrap(),
            "~rivdus-timret-tardet-paslux"
        );
        assert_eq!(hex2patp("d86003df88").unwrap(), "~dozret-tardet-paslux");
        assert_eq!(
            hex2patp("85613e0edad57f5fddc48d3f5808a628").unwrap(),
            "~rolruc-midwyl-hathes-palsym--fotnul-pagpur-nopful-lomtem"
        );
    }

    #[test]
    fn test_dec2patq() {
        assert_eq!(dec2patq("0").unwrap(), ".~zod");
        assert_eq!(dec2patq("213").unwrap(), ".~hes");
        assert_eq!(dec2patq("42345").unwrap(), ".~pindet");
        assert_eq!(dec2patq("1767345876").unwrap(), ".~davnyx-sopnes");
        assert_eq!(dec2patq("3825024628").unwrap(), ".~walnel-middut");
        assert_eq!(
            dec2patq("946825410821165233").unwrap(),
            ".~sabsep-ragsud-milnet-rigsud"
        );
        assert_eq!(
            dec2patq("946825410821165").unwrap(),
            ".~wes-mormep-ponrul-borbyn"
        );
        assert_eq!(
            dec2patq("123944546871737295045108034582151365233").unwrap(),
            ".~mornep-dinfyr-fognut-folheb-rivrem-wanteg-haprev-binter"
        );
    }

    #[test]
    fn test_dec2patp() {
        assert_eq!(dec2patp("0").unwrap(), "~zod");
        assert_eq!(dec2patp("213").unwrap(), "~hes");
        assert_eq!(dec2patp("42345").unwrap(), "~pindet");
        assert_eq!(dec2patp("1767345876").unwrap(), "~sabhut-mocsed");
        assert_eq!(dec2patp("3825024628").unwrap(), "~napdut-matful");
        assert_eq!(
            dec2patp("946825410821165233").unwrap(),
            "~sabsep-ragsud-sicsug-solmud"
        );
        assert_eq!(
            dec2patp("946825410821165").unwrap(),
            "~dozwes-mormep-simred-novtyd"
        );
        assert_eq!(
            dec2patp("123944546871737295045108034582151365233").unwrap(),
            "~mornep-dinfyr-fognut-folheb--rivrem-wanteg-haprev-binter"
        );
    }

    #[test]
    fn test_patp2syls() {
        assert!(matches!(patp2syls("~xxx"), Err(Error::InvalidSuffix(_))));
        assert!(matches!(
            patp2syls("~sampel-palxxx"),
            Err(Error::InvalidSuffix(_))
        ));
        assert!(matches!(
            patp2syls("~sampel-xxxnet"),
            Err(Error::InvalidPrefix(_))
        ));
        assert!(matches!(
            patp2syls("~dev-sampel-palnet"),
            Err(Error::ZeroPadRequired)
        ));
        assert!(matches!(patp2syls("~dozdev-sampel-palnet"), Ok(_)));
        assert!(matches!(
            patp2syls("sampel-palnet"),
            Err(Error::LeaderMissing(_))
        ));
        assert!(matches!(
            patp2syls(".~sampel-palnet"),
            Err(Error::LeaderMissing(_))
        ));
        assert!(matches!(
            patp2syls("~sampel-word-palnet"),
            Err(Error::InvalidSection(_))
        ));
        assert!(matches!(
            patp2syls("~rolruc-midwyl-hathes-palsym--fotnul-pagpur-nopful-lomtem"),
            Ok(_)
        ));
        assert!(matches!(
            patp2syls("~palsym--fotnul-pagpur-nopful-lomtem"),
            Ok(_)
        ));
        assert!(matches!(
            patp2syls("~rolruc-midwyl-hathes-palsym-fotnul-pagpur-nopful-lomtem"),
            Err(Error::InvalidSeparator)
        ));
        assert!(matches!(
            patp2syls("~rolruc-midwyl-hathes-palsym-fotnul-pagpur-nopful--lomtem"),
            Err(Error::InvalidSeparator)
        ));
    }

    #[test]
    fn test_patq2syls() {
        assert!(matches!(patq2syls(".~xxx"), Err(Error::InvalidSuffix(_))));
        assert!(matches!(
            patq2syls(".~sampel-palxxx"),
            Err(Error::InvalidSuffix(_))
        ));
        assert!(matches!(
            patq2syls(".~sampel-xxxnet"),
            Err(Error::InvalidPrefix(_))
        ));
        assert!(matches!(patq2syls(".~dev-sampel-palnet"), Ok(_)));
        assert!(matches!(patq2syls(".~dozdev-sampel-palnet"), Ok(_)));
        assert!(matches!(
            patq2syls("sampel-palnet"),
            Err(Error::LeaderMissing(_))
        ));
        assert!(matches!(
            patq2syls("~sampel-palnet"),
            Err(Error::LeaderMissing(_))
        ));
        assert!(matches!(
            patq2syls(".~sampel-word-palnet"),
            Err(Error::InvalidSection(_))
        ));
        assert!(matches!(
            patq2syls(".~rolruc-midwyl-hathes-palsym--fotnul-pagpur-nopful-lomtem"),
            Err(Error::InvalidSeparator)
        ));
        assert!(matches!(
            patq2syls(".~rolruc--midwyl-hathes--palsym-fotnul--pagpur-nopful--lomtem"),
            Err(Error::InvalidSeparator)
        ));
    }

    #[test]
    fn test_patp2dec() {
        assert_eq!(patp2dec("~binzod").unwrap(), "512");
        assert_eq!(patp2dec("~sampel-palnet").unwrap(), "1624961343");
        assert_eq!(
            patp2dec("~rolruc-midwyl-hathes-palsym--fotnul-pagpur-nopful-lomtem").unwrap(),
            "177292234920987225829036013324996224552"
        );
    }

    #[test]
    fn test_patq2dec() {
        assert_eq!(patq2dec(".~binzod").unwrap(), "512");
        assert_eq!(patq2dec(".~sampel-palnet").unwrap(), "74415951");
        assert_eq!(
            patq2dec(".~rolruc-midwyl-hathes-palsym-fotnul-pagpur-nopful-lomtem").unwrap(),
            "177292234920987225829036013324996224552"
        );
    }

    #[test]
    fn test_patp2hex() {
        assert_eq!(patp2hex("~binzod").unwrap(), "0200");
        assert_eq!(patp2hex("~sampel-palnet").unwrap(), "60daf13f");
        assert_eq!(
            patp2hex("~rolruc-midwyl-hathes-palsym--fotnul-pagpur-nopful-lomtem").unwrap(),
            "85613e0edad57f5fddc48d3f5808a628"
        );
    }

    #[test]
    fn test_patq2hex() {
        assert_eq!(patq2hex(".~binzod").unwrap(), "0200");
        assert_eq!(patq2hex(".~sampel-palnet").unwrap(), "046f7f4f");
        assert_eq!(
            patq2hex(".~rolruc-midwyl-hathes-palsym-fotnul-pagpur-nopful-lomtem").unwrap(),
            "85613e0edad57f5fddc48d3f5808a628"
        );
    }

    #[test]
    fn test_clan() {
        assert_eq!(clan("~zod").unwrap(), "galaxy");
        assert_eq!(clan("~binzod").unwrap(), "star");
        assert_eq!(clan("~sampel-palnet").unwrap(), "planet");
        assert_eq!(clan("~rolruc-midwyl-hathes-palsym").unwrap(), "moon");
        assert_eq!(
            clan("~rolruc-midwyl-hathes-palsym--fotnul-pagpur-nopful-lomtem").unwrap(),
            "comet"
        );
    }

    #[test]
    fn test_sein() {
        assert_eq!(sein("~dev").unwrap(), "~dev");
        assert_eq!(sein("~binhut").unwrap(), "~hut");
        assert_eq!(sein("~sampel-palnet").unwrap(), "~talpur");
        assert_eq!(
            sein("~rolruc-midwyl-hathes-palsym").unwrap(),
            "~hathes-palsym"
        );
        assert_eq!(
            sein("~rolruc-midwyl-hathes-palsym--fotnul-pagpur-nopful-lomtem").unwrap(),
            "~zod"
        );
    }
}
