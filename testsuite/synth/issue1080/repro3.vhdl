library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro3 is
  port (
    clk : std_logic;
    led : out std_logic);
end;

architecture behav of repro3 is
    constant LOOKUP_LEN   : integer := 6;
    constant LOOKUP_TABLE : unsigned(LOOKUP_LEN*8-1 downto 0) :=
        x"010205" & x"060708";

    signal brt : unsigned(7 downto 0) := (others => '0');

begin
  led <= brt (0);
    lookup_p : process(Clk)
        variable idx : integer range 0 to LOOKUP_LEN-1 := LOOKUP_LEN-1;
    begin
        if rising_edge(Clk) then
            brt <= lookup_table(8*idx+7 downto 8*idx);

            if idx /= 0 then
              idx := idx - 1;
            else
              idx := LOOKUP_LEN-1;
            end if;
        end if;
    end process;
end behav;
