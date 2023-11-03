use urbit_ob::*;

fn all_planets(star_val: u32) -> Vec<String> {
    let mut p_names: Vec<String> = Vec::with_capacity(0xffff);
    for value in 0x0001..=0xffff {
        let p_val: u32 = (value << 16) | star_val;
        let p_name = patp(p_val);
        p_names.push(p_name);
    }
    p_names
}

fn main() {
    println!("{:?}", patp2dec("~rondev"));
    println!("{:?}", patp2dec("~nodreb-borrus"));
    println!("{:?}", patp2dec("~dev-nodreb-borrus"));
    println!("{:?}", patp2hex("~binzod--rondev-master-martyr-finned"));
    println!("{:?}", patp2hex("~rondev-master-martyr-finned--rondev-master-martyr-finned--rondev-master-martyr-finned--rondev-master-martyr-finned"));
    //should fail from hyphens
    println!("{:?}", patq2hex(".~rondev-master-martyr-finned--rondev-master-martyr-finned--rondev-master-martyr-finned--rondev-master-martyr-finned"));
    println!("{:?}", hex2patp("b3"));
    println!("{:?}", hex2patq("b3"));
    println!("{:?}", hex2patq("60b3"));
    println!("{:?}", hex2patq("6b3"));
    println!("{:?}", hex2patq("06b3"));
    println!("{:?}", patq2hex(".~sigdev"));
    println!("{:?}", patq2hex(".~rondev"));
    println!("{:?}", hex2patp("10000"));
    println!("{:?}", hex2patp("e20200"));
    println!("{:?}", patp(0xe20200_u32));
    println!("{:?}", hex2patp("ffffffff"));
    println!("{:?}", hex2patq("e20200"));
    println!("{:?}", hex2patp("60b3"));
    println!("{:?}", hex2patp("06b3"));
    println!("{:?}", hex2patp("b360b3e20200e20200"));
    println!("{:?}", hex2patq("b360b3e20200e20200"));
    println!("{:?}", hex2patp("60b3ca7101995aca60b3ca7101995aca60b3ca7101995aca60b3ca7101995aca"));
    println!("{:?}", hex2patq("60b3ca7101995aca60b3ca7101995aca60b3ca7101995aca60b3ca7101995aca"));
    println!("{:?}", clan("~dev"));
    println!("{:?}", clan("~rondev"));
    println!("{:?}", clan("~nodreb-borrus"));
    println!("{:?}", clan("~binzod--rondev-master-martyr-finned"));
    println!("{:?}", sein("~dev"));
    println!("{:?}", sein("~rondev"));
    println!("{:?}", sein("~nodreb-borrus"));
    println!("{:?}", sein("~rondev-master-martyr-finned"));
    println!("{:?}", sein("~rondev-master-martyr-finned--rondev-master-martyr-finned--rondev-master-martyr-finned--rondev-master-martyr-finned"));
    println!("{:?}", PREFIX_VALUES.get("nod"));
    // println!("{:?}", muk(0xee281300, 2, &BigUint::from(0xcafebabe_u64)));

    for i in 24755..24756 {
        println!("{:?}", all_planets(i).last())
    }
}
