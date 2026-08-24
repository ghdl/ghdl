library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

package crc is
  constant M : integer := 4;
  constant N : integer := 32;

  type crc32table_t is array (integer range <>) of std_logic_vector(N - 1 downto 0);
  constant crc32table : crc32table_t (0 to M**2 - 1) := (
	x"00000000", 	x"105ec76f", 	x"20bd8ede", 	x"30e349b1",
	x"417b1dbc", 	x"5125dad3", 	x"61c69362", 	x"7198540d",
	x"82f63b78", 	x"92a8fc17", 	x"a24bb5a6", 	x"b21672c9",
	x"c38d26c4", 	x"d3d3e1ab", 	x"e330a81a", 	x"f36e6f75"
);

  type crc32 is protected
  impure function tablecrc4(a : std_logic_vector) return std_logic_vector;
  impure function initcrc4 return boolean;
  impure function finishcrc4(a : std_logic_vector) return std_logic_vector;
  impure function get return std_logic_vector;
end protected;
end package;

package body crc is
  type crc32 is protected body

  variable crcreg : std_logic_vector(N - 1 downto 0);

  impure function initcrc4 return boolean is
  begin
    crcreg := (others => '1');
    return true;
  end function;

  impure function finishcrc4(a : std_logic_vector) return std_logic_vector is
    variable temp : std_logic_vector(a'range) := (others => '1');
  begin
    return a xor temp;
  end function;


  impure function tablecrc4(a : std_logic_vector) return std_logic_vector is
    variable index : std_logic_vector(M - 1 downto 0);
  begin
    assert(a'length = M) report "Input of wrong length." severity failure;
    index := crcreg(M - 1 downto 0)  xor a;
    crcreg := crcreg srl M;
    crcreg := crcreg xor crc32table(to_integer(unsigned(index)));
    return crcreg;
  end function;

  impure function get return std_logic_vector is
  begin
    return crcreg;
  end function;

  end protected body;
end package body;
