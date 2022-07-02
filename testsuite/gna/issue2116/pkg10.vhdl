package gen0 is
generic(v:natural:=0);function get return natural;end gen0;package body gen0 is
function get return natural is begin return 0;end get;end gen0;package gen0 is
generic(package p is new k'g generic map(<>));function g return n;end gen0;package body n is
use g;function g return n is
begin
end;end;package p is new w;package g is new n generic map(0);entity tb is
end tb;architecture behav of b is
begin a;end behav;