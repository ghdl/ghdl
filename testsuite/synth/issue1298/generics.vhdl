library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity Params is
   generic (
      BOO : boolean:=FALSE;
      INT : integer:=0;
      LOG : std_logic:='0';
      VEC : std_logic_vector(7 downto 0):="00000000";
      STR : string:="ABCD";
      REA : real:=0.0
   );
   port (
      boo_o : out std_logic;
      int_o : out std_logic_vector(7 downto 0);
      log_o : out std_logic;
      vec_o : out std_logic_vector(7 downto 0);
      str_o : out std_logic;
      rea_o : out std_logic
   );
end entity Params;

architecture RTL of Params is
begin

   assert BOO=True       report "The boolean is not True" severity note;
   assert INT=255        report "The integer is not 255" severity note;
   assert LOG='1'        report "The std_logic is not '1'" severity note;
   assert VEC="11111111" report "The std_logic_vector is not 11111111" severity note;
   assert STR="WXYZ"     report "The string is not WXYZ" severity note;
--   assert REA=1.1        report "The real is not 1.1" severity note;

   boo_o <= '1' when BOO else '0';
   int_o <= std_logic_vector(to_unsigned(INT, 8));
   log_o <= LOG;
   vec_o <= VEC;
   str_o <= '1' when STR="WXYZ" else '0';
   rea_o <= '1' when REA=1.1 else '0';

end architecture RTL;
