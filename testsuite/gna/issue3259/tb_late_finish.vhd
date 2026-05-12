library std;
use std.env.finish;

entity tb_late_finish is
end entity tb_late_finish;

architecture behav of tb_late_finish is
begin

   main_p : process
   begin
      wait for 1 us;
      finish;
   end process main_p;

end architecture behav;
