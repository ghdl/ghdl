

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use STD.TEXTIO.ALL;

library work;
entity mem is
    Port(clk : IN std_logic);
end mem;

architecture bh of mem is
    type mem_block_t is protected
        procedure test;
    end protected;
    type mem_block_t is protected body

        procedure readHexByte(value : out std_logic_vector(7 downto 0)) is
        begin
        end procedure;

        procedure test is
            variable value : std_logic_vector(7 downto 0);
        begin
            readHexByte(value);
        end procedure;
    end protected body;
begin
end bh;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library work;

entity mem_tb is
begin
end entity;

architecture bh of mem_tb is
    signal clk : std_logic := '0';
begin
    MEM: entity work.mem port map (clk => clk);
end architecture;

