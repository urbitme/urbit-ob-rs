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
    for i in 24755..24756 {
        println!("{:?}", all_planets(i).last())
    }
}
