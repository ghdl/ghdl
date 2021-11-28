-- Machine generated from ram.img.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package bootrom is
  type rom_t is array (0 to 127) of std_logic_vector(31 downto 0);
  constant rom : rom_t := (
    x"00000110",
    x"00001ffc",
    x"00000110",
-- more stuff, doesn't matter as long as it fits...
    x"23811fac",
    x"00afffac",
    others => x"00000000" );

end package;

package body bootrom is
end package body;

-- A simple pre-initalized RAM, which reads from a binary file at synthesis time
-- single 32 bit read/write port.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity simple_ram is
  generic (
    -- 32-bit read/write port.  ADDR_WIDTH is in bytes, not words.
    ADDR_WIDTH : integer := 8 -- default 32k
    );
  port (
    clk : in std_logic;

    en : in std_logic;
    raddr : in std_logic_vector(ADDR_WIDTH - 3 downto 0);
    dout : out std_logic_vector(31 downto 0);

    we : in std_logic_vector(3 downto 0);
    waddr : in std_logic_vector(ADDR_WIDTH - 3 downto 0);
    din : in std_logic_vector(31 downto 0)
    );
end simple_ram;

use work.bootrom.all;

architecture behavioral of simple_ram is
  constant NUM_WORDS : integer :=  2**(ADDR_WIDTH - 2);
  signal ram : rom_t := work.bootrom.rom;  -- FIXME init internal error
begin

  process (clk, en)
    variable read : std_logic_vector(31 downto 0);
  begin
    if clk'event and clk = '1' and en = '1' then -- Unsupported: clock enable
      if we(3) = '1' then
        ram(to_integer(unsigned(waddr)))(31 downto 24) <= din(31 downto 24);
      end if;
      if we(2) = '1' then
        ram(to_integer(unsigned(waddr)))(23 downto 16) <= din(23 downto 16);
      end if;
      if we(1) = '1' then
        ram(to_integer(unsigned(waddr)))(15 downto 8 ) <= din(15 downto 8 );
      end if;
      if we(0) = '1' then
        ram(to_integer(unsigned(waddr)))(7  downto 0 ) <= din(7  downto 0 );
      end if;
      read := ram(to_integer(unsigned(raddr)));
      dout <= read;
    end if;
  end process;
end behavioral;
