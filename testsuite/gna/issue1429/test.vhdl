use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity;

architecture a of test is
  constant SUB_v : bit_vector(63 downto 0) := (others => '1');
  constant I0    : bit_vector(63 downto 0) := X"AAAAAAAAAAAAAAAA";
  constant I0a   : bit_vector(63 downto 0) := I0 xor X"FFFFFFFFFFFFFFFF";
  constant I0b   : bit_vector(63 downto 0) := I0 xor SUB_v;

  impure function I0f return bit_vector is
    variable ret : bit_vector(63 downto 0);
  begin
    ret := X"AAAAAAAAAAAAAAAA";
    ret := ret xor SUB_v;
    return ret;
  end function;

  constant I0c   : bit_vector(63 downto 0) := I0f;
begin
  process is
    variable la, lb, lc : line;
  begin
    hwrite(la, I0a);
    report la.all;
    hwrite(lc, I0c);
    report lc.all;
    assert I0a = I0c severity failure;
    assert I0b = I0c severity failure;
    wait;
  end process;
end architecture a;
