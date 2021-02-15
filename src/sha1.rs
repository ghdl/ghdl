use std::fmt;

const H0: u32 = 0x67452301;
const H1: u32 = 0xEFCDAB89;
const H2: u32 = 0x98BADCFE;
const H3: u32 = 0x10325476;
const H4: u32 = 0xC3D2E1F0;

const K0: u32 = 0x5A827999;
const K1: u32 = 0x6ED9EBA1;
const K2: u32 = 0x8F1BBCDC;
const K3: u32 = 0xCA62C1D6;

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Sha1State {
    a: u32,
    b: u32,
    c: u32,
    d: u32,
    e: u32,
}

impl fmt::Debug for Sha1State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:x} {:x} {:x} {:x} {:x}", self.a, self.b, self.c, self.d, self.e)
    }
}

fn rot(v: u32, amount: usize) -> u32 {
    (v << amount) | (v >> (32 - amount))
}

fn p0(v: Sha1State, w: [u32;16], t: usize) -> Sha1State {
    let temp = rot(v.a, 5)
        .wrapping_add((v.b & v.c) | (!v.b & v.d))
        .wrapping_add(v.e)
        .wrapping_add(w[t])
        .wrapping_add(K0);
    Sha1State{a: temp, b: v.a, c: rot(v.b, 30), d: v.c, e: v.d}
}

fn p0b(v: Sha1State, w: &mut [u32;16], t: usize) -> Sha1State {
    w[t] = rot(w[(t+13) & 15] ^ w[(t+8) & 15] ^ w[(t+2) & 15] ^ w[t], 1);
    let temp = rot(v.a, 5)
        .wrapping_add((v.b & v.c) | (!v.b & v.d))
        .wrapping_add(v.e)
        .wrapping_add(w[t])
        .wrapping_add(K0);
    Sha1State{a: temp, b: v.a, c: rot(v.b, 30), d: v.c, e: v.d}
}

fn p1(v: Sha1State, w: &mut [u32;16], t: usize) -> Sha1State {
    w[t] = rot(w[(t+13) & 15] ^ w[(t+8) & 15] ^ w[(t+2) & 15] ^ w[t], 1);
    let temp = rot(v.a, 5)
        .wrapping_add(v.b ^ v.c ^ v.d)
        .wrapping_add(v.e)
        .wrapping_add(w[t])
        .wrapping_add(K1);
    Sha1State{a: temp, b: v.a, c: rot(v.b, 30), d: v.c, e: v.d}
}

fn p2(v: Sha1State, w: &mut [u32;16], t: usize) -> Sha1State {
    w[t] = rot(w[(t+13) & 15] ^ w[(t+8) & 15] ^ w[(t+2) & 15] ^ w[t], 1);
    let temp = rot(v.a, 5)
        .wrapping_add((v.b & v.c) | (v.b & v.d)| (v.c & v.d))
        .wrapping_add(v.e)
        .wrapping_add(w[t & 15])
        .wrapping_add(K2);
    Sha1State{a: temp, b: v.a, c: rot(v.b, 30), d: v.c, e: v.d}
}

fn p3(v: Sha1State, w: &mut [u32;16], t: usize) -> Sha1State {
    w[t] = rot(w[(t+13) & 15] ^ w[(t+8) & 15] ^ w[(t+2) & 15] ^ w[t], 1);
    let temp = rot(v.a, 5)
        .wrapping_add(v.b ^ v.c ^ v.d)
        .wrapping_add(v.e)
        .wrapping_add(w[t])
        .wrapping_add(K3);
    Sha1State{a: temp, b: v.a, c: rot(v.b, 30), d: v.c, e: v.d}
}

fn process_block(blk: &mut Sha1State, dat: [u32;16]) {
    let mut w: [u32;16] = dat;

    // a. Divide Mi into 16 words W0, W1, ..., W15 where W0 is the
    // left-most word.

    // b. For t = 16 to 79...

    // c. Let A=H0, ...
    let mut v = *blk;

    // d. For t = 0 to 79 do ...
    //   0..=19
    v = p0(v, w, 0);
    v = p0(v, w, 1);
    v = p0(v, w, 2);
    v = p0(v, w, 3);
    v = p0(v, w, 4);
    v = p0(v, w, 5);
    v = p0(v, w, 6);
    v = p0(v, w, 7);
    v = p0(v, w, 8);
    v = p0(v, w, 9);
    v = p0(v, w, 10);
    v = p0(v, w, 11);
    v = p0(v, w, 12);
    v = p0(v, w, 13);
    v = p0(v, w, 14);
    v = p0(v, w, 15);
    v = p0b(v, &mut w, 0);
    v = p0b(v, &mut w, 1);
    v = p0b(v, &mut w, 2);
    v = p0b(v, &mut w, 3);
    //  20..=39
    v = p1(v, &mut w, 4);
    v = p1(v, &mut w, 5);
    v = p1(v, &mut w, 6);
    v = p1(v, &mut w, 7);
    v = p1(v, &mut w, 8);
    v = p1(v, &mut w, 9);
    v = p1(v, &mut w, 10);
    v = p1(v, &mut w, 11);
    v = p1(v, &mut w, 12);
    v = p1(v, &mut w, 13);
    v = p1(v, &mut w, 14);
    v = p1(v, &mut w, 15);
    v = p1(v, &mut w, 0);
    v = p1(v, &mut w, 1);
    v = p1(v, &mut w, 2);
    v = p1(v, &mut w, 3);
    v = p1(v, &mut w, 4);
    v = p1(v, &mut w, 5);
    v = p1(v, &mut w, 6);
    v = p1(v, &mut w, 7);
    //  40..=59
    v = p2(v, &mut w, 8);
    v = p2(v, &mut w, 9);
    v = p2(v, &mut w, 10);
    v = p2(v, &mut w, 11);
    v = p2(v, &mut w, 12);
    v = p2(v, &mut w, 13);
    v = p2(v, &mut w, 14);
    v = p2(v, &mut w, 15);
    v = p2(v, &mut w, 0);
    v = p2(v, &mut w, 1);
    v = p2(v, &mut w, 2);
    v = p2(v, &mut w, 3);
    v = p2(v, &mut w, 4);
    v = p2(v, &mut w, 5);
    v = p2(v, &mut w, 6);
    v = p2(v, &mut w, 7);
    v = p2(v, &mut w, 8);
    v = p2(v, &mut w, 9);
    v = p2(v, &mut w, 10);
    v = p2(v, &mut w, 11);
    //  60..=79
    v = p3(v, &mut w, 12);
    v = p3(v, &mut w, 13);
    v = p3(v, &mut w, 14);
    v = p3(v, &mut w, 15);
    v = p3(v, &mut w, 0);
    v = p3(v, &mut w, 1);
    v = p3(v, &mut w, 2);
    v = p3(v, &mut w, 3);
    v = p3(v, &mut w, 4);
    v = p3(v, &mut w, 5);
    v = p3(v, &mut w, 6);
    v = p3(v, &mut w, 7);
    v = p3(v, &mut w, 8);
    v = p3(v, &mut w, 9);
    v = p3(v, &mut w, 10);
    v = p3(v, &mut w, 11);
    v = p3(v, &mut w, 12);
    v = p3(v, &mut w, 13);
    v = p3(v, &mut w, 14);
    v = p3(v, &mut w, 15);

    // e. Let...
    blk.a = blk.a.wrapping_add(v.a);
    blk.b = blk.b.wrapping_add(v.b);
    blk.c = blk.c.wrapping_add(v.c);
    blk.d = blk.d.wrapping_add(v.d);
    blk.e = blk.e.wrapping_add(v.e);
}

fn to_u32x16(v: &[u8]) -> [u32;16] {
    let mut res: [u32;16] = [0; 16];
    for i in 0..16 {
        res[i] = ((v[i*4] as u32) << 24) | ((v[i*4 + 1] as u32) << 16) | ((v[i*4 + 2] as u32) << 8) | (v[i*4 + 3] as u32);
    }
    res
}

pub fn sha1(dat: &[u8]) -> Sha1State {
    //  Init
    let mut res = Sha1State{a: H0, b: H1, c: H2, d: H3, e: H4};

    let len = dat.len();
    let mut off: usize = 0;

    while off + 64 < len {
        process_block(&mut res, to_u32x16(&dat[off..off + 64]));
        off += 64;
    }
    let mut pad: [u8;64] = [0;64];
    for i in off..len {
        pad[i - off] = dat[i];
    }
    pad[len - off] = 0x80;
    let mut padu32 = to_u32x16(&pad[..]);

    let lenh = ((len * 8) >> 32) as u32;
    let lenl = ((len * 8) & 0xffff_ffff) as u32;

    if len - off < 55 {
        padu32[14] = lenh;
        padu32[15] = lenl;
        process_block(&mut res, padu32);
    } else {
        process_block(&mut res, padu32);
        padu32 = [0;16];
        padu32[14] = lenh;
        padu32[15] = lenl;
        process_block(&mut res, padu32);
    }
    res
}

pub fn sha1_str(s: &str) -> Sha1State {
    sha1(s.as_bytes())
}

#[cfg(test)]
mod tests {
    use super::Sha1State;

    #[test]
    fn test1() {
        let r = super::sha1_str("hello world");
        println!("{:?}", r);

        let r = super::sha1_str("abc");
        assert_eq!(r, Sha1State{a: 0xA9993E36, b: 0x4706816A,c: 0xBA3E2571, d: 0x7850C26C, e: 0x9CD0D89D});
        println!("{:?}", r);

        let r = super::sha1_str("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
        assert_eq!(r, Sha1State{a: 0x84983E44, b: 0x1C3BD26E, c: 0xBAAE4AA1, d: 0xF95129E5, e: 0xE54670F1});
        println!("{:?}", r);
    }
}