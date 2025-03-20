library ieee;
use ieee.std_logic_1164.all;

package mem_pkg is
  type mem32_type is array (natural range <>) of std_logic_vector(31 downto 0);

  constant mem1 : mem32_type :=
    (
      x"1234_5678",
      x"0000_0000",
      x"1111_1111",
      x"2222_2222",
      x"3333_3333",
      x"4444_4444",
      x"5555_5555",
      x"6666_6666",
      x"7777_7777",
      x"8888_8888",
      x"9999_9999",
      x"aaaa_aaaa",
      x"bbbb_bbbb",
      x"cccc_cccc"
      );
end mem_pkg;
