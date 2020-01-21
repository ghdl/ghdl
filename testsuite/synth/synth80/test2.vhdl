library ieee;
use ieee.std_logic_1164.all;

entity testcase is
    generic (
        init_bit : std_logic := '1'
    );
    port (o : out std_logic_vector (2 downto 0));
end testcase;

architecture rtl of testcase is
    -- assigning generic to multiple parts of std_logic_vector breaks ghdlsynth
    signal test_assign_vector : std_logic_vector(2 downto 0) := init_bit & "0" & init_bit;
begin
  o <= test_assign_vector;
end rtl;
