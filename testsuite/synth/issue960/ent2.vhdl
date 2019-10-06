library ieee;
use ieee.std_logic_1164.all;

entity ent2 is
    port (
      clk : in std_logic;
      o : out std_logic
    );
end;

architecture a of ent2 is
    procedure inv(signal s : inout std_logic) is
    begin
        s <= not s;
    end procedure;

    signal test : std_logic := '0';
begin
    process(clk)
    begin
      if rising_edge(clk) then
        inv(test);
      end if;
    end process;
    o <= test;
end;
