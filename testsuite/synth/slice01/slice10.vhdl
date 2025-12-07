library ieee;
use ieee.std_logic_1164.all;

entity slice10 is
  generic (w: natural := 4);
  port (clk : std_logic;
        dat : std_logic_vector (7 downto 0);
        mask : std_logic_vector (1 downto 0);
        res : out std_logic_vector (7 downto 0));
end;

architecture behav of slice10 is
begin
  process
    variable hi, lo : integer;
  begin
    hi := 0;
    lo := -1;
    assert dat(hi downto lo) /= "00";
wait;
  end process;
end behav;
