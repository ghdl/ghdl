library ieee;
use ieee.std_logic_1164.all;

entity var01 is
  port (clk : std_logic;
        mask : std_logic_vector (3 downto 0);
        val : std_logic_vector (31 downto 0);
        res : out std_logic_vector (31 downto 0));
end var01;

architecture behav of var01 is
begin
  process (clk)
    variable hi, lo : natural;
  begin
    if rising_edge (clk) then
      for i in 0 to 3 loop
        if mask (i) = '1' then
          lo := i * 8;
          hi := lo + 7;
          res (hi downto lo) <= val (hi downto lo);
        end if;
      end loop;
    end if;
  end process;
end behav;
