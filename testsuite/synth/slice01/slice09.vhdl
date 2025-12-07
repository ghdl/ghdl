library ieee;
use ieee.std_logic_1164.all;

entity slice09 is
  generic (w: natural := 4);
  port (clk : std_logic;
        dat : std_logic_vector (7 downto 0);
        mask : std_logic_vector (1 downto 0);
        res : out std_logic_vector (7 downto 0));
end slice09;

architecture behav of slice09 is
begin
  process
    variable hi, lo : integer;
  begin
    hi := 9;
    lo := 7;
    assert dat(hi downto lo) /= "00";
    wait;
  end process;
end behav;
