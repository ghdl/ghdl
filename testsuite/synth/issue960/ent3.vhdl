library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
      clk : in std_logic;
      o : out std_logic
    );
end;

architecture a of ent is
  function inv(s : std_logic) return std_logic is
  begin
    return not s;
  end inv;

    signal test : std_logic;
begin
    process(clk)
    begin
      if rising_edge(clk) then
        test <= inv(test);
      end if;
    end process;
    o <= test;
end;
