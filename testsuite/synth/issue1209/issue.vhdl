library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_arith.ALL;

entity issue is
 generic(
  data_width : integer := 4
 );
 port(
  clk : in  std_logic;
  di  : in  std_logic_vector(data_width - 1 downto 0);
  do  : out std_logic_vector(data_width - 1 downto 0)
 );
end issue;

architecture behavioral of issue is
    constant base_const : std_logic_vector(data_width - 1 downto 0) := conv_std_logic_vector(3, data_width);
    constant new_const  : std_logic_vector(data_width - 1 downto 0) := base_const + "0100";
begin
    do <= new_const;
end behavioral;
