library ieee;
use ieee.std_logic_1164.all;

entity repro3_1 is
  port (
    clk : std_logic;
    led : out std_logic_vector(7 downto 0));
end;

architecture behav of repro3_1 is
    constant LOOKUP_LEN   : integer := 6;
    constant LOOKUP_TABLE : std_logic_vector(LOOKUP_LEN*8-1 downto 0) :=
      x"010205" & x"060708";
    -- x"010205060708";
    --  -> const_bit (0x5060708, 0x102)
begin
    lookup_p : process(Clk)
        variable idx : integer range 0 to LOOKUP_LEN-1 := LOOKUP_LEN-1;
    begin
        if rising_edge(Clk) then
          led <= lookup_table(8*idx+7 downto 8*idx);

            if idx /= 0 then
              idx := idx - 1;
            else
              idx := LOOKUP_LEN-1;
            end if;
        end if;
    end process;
end behav;
