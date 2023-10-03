library ieee;
use ieee.std_logic_1164.all;

entity nomem2 is
  generic (l : natural := 1);
  port (inp : std_logic_vector(l - 1 downto 0);
        v : std_logic;
        clk : std_logic;
        off : natural;
        off2 : natural;
        res : out std_logic_vector(l - 1 downto 0));
end;

architecture arch of nomem2 is
  type mem_t is array (natural range <>) of std_logic_vector(0 downto 0);
  signal m : mem_t (0 downto 0);
begin
  res(0) <= m(off)(off2);
  
  process (clk)
  begin
    if rising_edge(clk) then
      m (off2)(off2) <= v;
    end if;
  end process;
end;
