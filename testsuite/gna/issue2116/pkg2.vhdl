package gen0 is
generic(v:natural:=0);function get return natural;end gen0;package body gen0 is
function get return natural is
begin
return 0;end get;end gen0;package gen2 is generic(package pkg is new work.gen0 generic map(<>));function get2 return natural;end gen2;package body gen2 is
use pkg.all;function get2 return natural is
begin
return get;end get2;end;package p is new work.gen0;package g is new work.gen2 generic map(0);entity b is
end;architecture behav of b is
begin end behav;