library ieee;
use ieee.std_logic_1164.all;

entity test is
    port (
        input_i : in std_ulogic_vector;
        output1_o : out std_ulogic_vector;
        output2_o : out std_ulogic_vector
    );
end;

architecture behav of test is
begin
end;

library ieee;
use ieee.std_logic_1164.all;

entity tb is
end;

architecture behav of tb is
  signal my_input : std_ulogic_vector (7 downto 0);
  signal my_output : std_ulogic_vector(3 downto 0);
begin
thing : entity work.test port map (
    input_i => my_input,
    output1_o => my_output,
    output2_o(0) => open
);
end;
