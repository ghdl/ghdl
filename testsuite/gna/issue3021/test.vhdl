library ieee;
use ieee.std_logic_1164.all;
entity sub_module is
    generic(
        type t_my_type
    );
end entity sub_module;
architecture rtl of sub_module is
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;
entity test is
    generic (
        constant g_op1_width : natural := 8;
        constant g_op2_width : natural := 4
    );
    port (
        op1_i : in  std_logic_vector(g_op1_width-1 downto 0);
        op2_i : in  std_logic_vector(g_op2_width-1 downto 0)
    );
end entity test;
architecture rtl of test is
    type t_my_matrix is array (g_op2_width-1 downto 0) of std_logic_vector(g_op1_width-1 downto 0);
    component sub_module
    generic(
        type t_my_type
    );
    end component;
begin
    sub_inst: sub_module;
end architecture;
