library ieee;                                             
   use ieee.std_logic_1164.all;   
   use ieee.numeric_std.all;      

entity top is
end entity ; 

architecture arch_top of top is

    type t_slv_array is array(natural range <>) of std_logic_vector;

    signal test : t_slv_array(0 to 2)(7 downto 0);

begin

end architecture arch_top;
