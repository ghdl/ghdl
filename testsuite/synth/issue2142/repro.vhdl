library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro is
  port (radr : unsigned (0 downto 0);
        v : out std_logic_vector(7 downto 0);
        we : std_logic;
        clk : std_logic);
end repro;

architecture behav of repro is
  type t_mem is array (0 to 0) of std_logic_vector(7 downto 0);
  signal m : t_mem;
begin
  process (clk)
  begin
    if rising_edge (clk) then
      if we = '1' then
        m(0) <= not m(0);
      else
        v <= m (to_integer (radr));
      end if;
    end if;
  end process;
end behav;
