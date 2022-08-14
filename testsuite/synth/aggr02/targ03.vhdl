library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity targ03 is
  port (rdat : out std_logic_vector (7 downto 0);
        rv : out std_logic;
        wdat : std_logic_vector (7 downto 0);
        wval : std_logic;
        wen : std_logic;
        clk : std_logic);
end targ03;

architecture behav of targ03 is
  type memdat is record
    d1 : std_logic_vector(7 downto 0);
    v : std_logic;
  end record;
begin
  process (clk)
    variable m : memdat;
  begin
    if rising_edge (clk) then
      if wen = '1' then
        m := (wdat, wval);
      end if;
      (rdat, rv) <= m;
    end if;
  end process;
end behav;
