package gen0 is
generic(v:natural:=0);function get return natural;end gen0;package body gen0 is
function get return natural is begin return 0;end get;end;package gen2 is generic(package pkg is new work.gen0 generic map(<>));function get2 return natural;end gen2;package body gen2 is use pkg.all;function get2 return natural is begin return get;end;end;package g is new work.gen0;package p is new work.gen2 generic map(0);architecture behav of b is
begin end behav;