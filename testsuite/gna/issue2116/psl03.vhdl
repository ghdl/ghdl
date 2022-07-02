package gen0 is
generic(v:natural:=0);function get return natural;end;package body gen0 is
function get return natural is
begin return 0;end;end;package gen2 is generic(package pkg is new work.gen0 generic map(<>));function get2 return natural;end;package body gen2 is use pkg.all;function get2 return natural is begin return get;end get2;end;package pkg0 is new work.gen0;package p is new work.gen2 generic map(work.pkg0);entity tb is
end;architecture behav of tb is
begin assert 0!;end behav;