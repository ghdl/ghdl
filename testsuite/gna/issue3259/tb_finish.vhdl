library std;
use std.env.finish;

entity tb_finish is
end entity tb_finish;

architecture behav of tb_finish is
begin

   main_p : process
   begin
      wait for 1 ns;
      finish;
   end process main_p;

end architecture behav;
