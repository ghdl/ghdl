library ieee;
use ieee.std_logic_1164.all;

package pkg is
   procedure generic_passthrough
      generic (
         type G_TYPE
      )
      parameter (
         signal s_in  : in G_TYPE;
         signal s_out : out G_TYPE
      );

end package;

package body pkg is
   procedure generic_passthrough
      generic (
         type G_TYPE
      )
      parameter (
         signal s_in  : in G_TYPE;
         signal s_out : out G_TYPE
      )
   is
   begin
      s_out <= s_in;
   end procedure;
end package body;

