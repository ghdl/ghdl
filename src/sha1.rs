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

fn rot(v: u32, amount: usize) -> u32 {
    (v << amount) | (v >> (32 - amount))
}

fn f0(b: u32, c: u32, d: u32) -> u32 {
    (b & c) | (!b & d)
}

fn f1(b: u32, c: u32, d: u32) -> u32 {
    b ^ c ^ d
}

fn f2(b: u32, c: u32, d: u32) -> u32 {
    (b & c) | (b & d)| (c & d)
}

fn f3(b: u32, c: u32, d: u32) -> u32 {
    b ^ c ^ d
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Sha1Block {
    a: u32,
    b: u32,
    c: u32,
    d: u32,
    e: u32,
}

impl fmt::Debug for Sha1Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:x} {:x} {:x} {:x} {:x}", self.a, self.b, self.c, self.d, self.e)
    }
}

fn permute(v: &mut Sha1Block, temp: u32) {
    v.e = v.d;
    v.d = v.c;
    v.c = rot(v.b, 30);
    v.b = v.a;
    v.a = temp;
}

fn process_block(blk: &mut Sha1Block, dat: [u32;16]) {
    let mut w: [u32;80] = [0;80];
    for i in 0..16 {
        w[i] = dat[i];
    }

    // a. Divide Mi into 16 words W0, W1, ..., W15 where W0 is the
    // left-most word.

    // b. For t = 16 to 79...
    for t in 16..=79 {
        w[t] = rot(w[t-3] ^ w[t-8] ^ w[t-14] ^ w[t-16], 1);
    }

    // c. Let A=H0, ...
    let mut v = *blk;

    // d. For t = 0 to 79 do ...
    for t in 0..=19 {
        let temp = rot(v.a, 5).wrapping_add(f0(v.b, v.c, v.d)).wrapping_add(v.e).wrapping_add(w[t]).wrapping_add(K0);
        permute(&mut v, temp);
    }
    for t in 20..=39 {
        let temp = rot(v.a, 5).wrapping_add(f1(v.b, v.c, v.d)).wrapping_add(v.e).wrapping_add(w[t]).wrapping_add(K1);
        permute(&mut v, temp);
    }
    for t in 40..=59 {
        let temp = rot(v.a, 5).wrapping_add(f2(v.b, v.c, v.d)).wrapping_add(v.e).wrapping_add(w[t]).wrapping_add(K2);
        permute(&mut v, temp);
    }
    for t in 60..=79 {
        let temp = rot(v.a, 5).wrapping_add(f3(v.b, v.c, v.d)).wrapping_add(v.e).wrapping_add(w[t]).wrapping_add(K3);
        permute(&mut v, temp);
    }

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

pub fn sha1(dat: &[u8]) -> Sha1Block {
    //  Init
    let mut res = Sha1Block{a: H0, b: H1, c: H2, d: H3, e: H4};

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

pub fn sha1_str(s: &str) -> Sha1Block {
    sha1(s.as_bytes())
}

#[cfg(test)]
mod tests {
    use super::Sha1Block;

    #[test]
    fn test1() {
        let r = super::sha1_str("hello world");
        println!("{:?}", r);

        let r = super::sha1_str("abc");
        assert_eq!(r, Sha1Block{a: 0xA9993E36, b: 0x4706816A,c: 0xBA3E2571, d: 0x7850C26C, e: 0x9CD0D89D});
        println!("{:?}", r);

        let r = super::sha1_str("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
        assert_eq!(r, Sha1Block{a: 0x84983E44, b: 0x1C3BD26E, c: 0xBAAE4AA1, d: 0xF95129E5, e: 0xE54670F1});
        println!("{:?}", r);
    }
}