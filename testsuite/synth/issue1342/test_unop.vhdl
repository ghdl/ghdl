library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity test_unop is
    Port (vec_in: in STD_LOGIC_VECTOR(31 downto 0);
         reduce_out_and: out STD_LOGIC;
         reduce_out_nand: out STD_LOGIC;
         reduce_out_or: out STD_LOGIC;
         reduce_out_nor: out STD_LOGIC;
         reduce_out_xor: out STD_LOGIC;
         reduce_out_xnor: out STD_LOGIC);
end entity;

architecture Behavioral of test_unop is
begin
    process(vec_in) is
    begin
    reduce_out_and <= and vec_in;
    reduce_out_nand <= nand vec_in;
    reduce_out_or <= or vec_in;
    reduce_out_nor <= nor vec_in;
    reduce_out_xor <= xor vec_in;
    reduce_out_xnor <= xnor vec_in;
    end process;
end Behavioral;
