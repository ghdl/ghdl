library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity slice05 is
  port (clk : std_logic;
        dat : std_logic_vector (7 downto 0);
        mask : std_logic_vector (1 downto 0);
        res : out std_logic_vector (7 downto 0));
end;

architecture behav of slice05 is
  subtype nul_t is natural range 0 to 0;
  signal z : nul_t;

  procedure wr(d : inout std_logic_vector(7 downto 0);
               v : std_logic_vector(7 downto 0);
               p : nul_t) is
  begin
    d (p*3 + 7 downto p*3) := v;
  end wr;
begin
  z <= to_integer(unsigned(mask));

  process(clk)
    variable mem : std_logic_vector (7 downto 0);
  begin
    if rising_edge (clk) then
      wr (mem, dat, z + 0);
      res <= mem;
    end if;
  end process;
end behav;
