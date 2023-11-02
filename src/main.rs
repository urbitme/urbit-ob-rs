use urbit_ob::*;

fn main() {
    println!("{:?}", patp2dec("~rondev"));
    println!("{:?}", patp2dec("~nodreb-borrus"));
    println!("{:?}", patp2dec("~dev-nodreb-borrus"));
    println!("{:?}", patp2hex("~binzod--rondev-master-martyr-finned"));
    println!("{:?}", hex2patp("b3"));
    println!("{:?}", hex2patq("b3"));
    println!("{:?}", hex2patq("60b3"));
    println!("{:?}", hex2patq("6b3"));
    println!("{:?}", hex2patq("06b3"));
    println!("{:?}", patq2hex(".~sigdev"));
    println!("{:?}", patq2hex(".~rondev"));
    println!("{:?}", hex2patp("e20200"));
    println!("{:?}", hex2patq("e20200"));
    println!("{:?}", hex2patp("60b3"));
    println!("{:?}", hex2patp("06b3"));
    println!("{:?}", hex2patp("b360b3e20200e20200"));
    println!("{:?}", hex2patq("b360b3e20200e20200"));
    println!("{:?}", clan("~dev"));
    println!("{:?}", clan("~rondev"));
    println!("{:?}", clan("~nodreb-borrus"));
    println!("{:?}", clan("~binzod--rondev-master-martyr-finned"));
    println!("{:?}", sein("~dev"));
    println!("{:?}", sein("~rondev"));
    println!("{:?}", sein("~nodreb-borrus"));
    println!("{:?}", sein("~rondev-master-martyr-finned"));
    println!("{:?}", PREFIX_VALUES.get("nod"));
    // println!("{:?}", muk(0xee281300, 2, &BigUint::from(0xcafebabe_u64)));
}
