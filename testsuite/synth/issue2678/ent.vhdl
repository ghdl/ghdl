library ieee;
   use ieee.std_logic_1164.all;
   use ieee.numeric_std_unsigned.all;

entity ent is
   port (
      clk_i  : in    std_logic;
      col_i  : in    natural;
      addr_o : out   std_logic_vector(1 downto 0)
   );
end entity ent;

architecture synth of ent is

   constant C_ROW_SIZE : natural  := 4;

   pure function log2 (
      arg : natural
   ) return natural is
   begin
      --
      for i in 0 to arg loop
         if 2 ** i >= arg then
            return i;
         end if;
      end loop;

      return -1;
   end function log2;

   constant C_ADDR_SIZE : natural := log2(C_ROW_SIZE);

   signal   addr : std_logic_vector(C_ADDR_SIZE - 1 downto 0);
   signal   col  : natural range 0 to C_ROW_SIZE - 1;

begin

   col    <= col_i;
   addr   <= to_stdlogicvector(col, C_ADDR_SIZE);
   addr_o <= addr(1 downto 0);

end architecture synth;
