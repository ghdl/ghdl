package test3 is
  type t_prot is protected
    procedure proc;
  end protected t_prot;
end package;
package body test3 is
  type t_prot is protected body
    procedure proc is
    begin
      report "proc";
    end proc;
  end protected body t_prot;
end package body;

package test2 is
  shared variable shared_var : work.test3.t_prot;
end package;

package test1 is
  alias shared_var_2 is work.test2.shared_var;
end package;

use work.test1.all;

entity test is
end test;

architecture behav of test is
begin
  process
  begin
    shared_var_2.proc;
    report "done";
    wait;
  end process;
end behav;

