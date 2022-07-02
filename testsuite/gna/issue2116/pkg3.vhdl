package gen0 is
generic(v:natural:=0);function get return natural;end gen0;package body gen0 is
function get return natural is
begin return 0;end get;end gen0;package n is generic(package p is new k'g generic map(<>));function g return n;end;package body gen0 is
use k;end gen0;package p is new w;package g is new k generic map(0);entity b is
end;architecture behav of b is
begin
end behav;