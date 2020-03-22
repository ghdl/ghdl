library IEEE;
use IEEE.std_logic_1164.ALL;

entity A is
  port (
    clk : in std_logic;
        input : in std_logic;
        output : out std_logic
    );

end entity A;


architecture RTL of A is
begin

    not_proc : process (clk)

        variable not_input : std_logic := '0';

    begin
        if rising_edge(clk) then
      not_input := not input;
    end if;

    output <= not_input;
    end process;
end RTL;
