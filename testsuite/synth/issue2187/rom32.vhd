-- A simple synthesized ROM, which reads from a binary file at synthesis time
-- single 32 bit read port.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.rawfile_pack.all;

entity simple_rom32 is
  generic (
    -- 32-bit read port.  ADDR_WIDTH is in bytes, not words.
    ADDR_WIDTH : integer := 8 -- default 256 bytes
    );
  port (
    raddr : in std_logic_vector(ADDR_WIDTH - 3 downto 0);
    do : out std_logic_vector(31 downto 0));
end simple_rom32;

architecture behavioral of simple_rom32 is
  constant NUM_WORDS : integer :=  2**(ADDR_WIDTH - 2);
  signal rom : rom32_t(0 to NUM_WORDS-1) := fread_elf("image.elf", NUM_WORDS);
begin
  do <= rom(to_integer(unsigned(raddr)));
end behavioral;
