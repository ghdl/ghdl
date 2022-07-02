package gen0 is
generic(v:natural:=0);function get return natural;end;package body gen0 is
function get return natural is begin return 0;end;end gen0;package gen0 is
generic(package g is new k'g generic map(0));function g return n;end gen0;package body n is
use g;function g return n is
begin
end;end;package p is new w;package g is new o generic map(0);entity tb is
end tb;architecture behav of b is
begin end behav;