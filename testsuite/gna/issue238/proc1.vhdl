entity proc1 is
end;

use work.pkg.all;

architecture behav of proc1 is
  procedure proc (v : inout rec) is
  begin
    v.a := 5;
    assert v.a = 5 severity failure;

    v.s := "Good";
    assert v.a = 5 severity failure;
    assert v.s = "Good" severity failure;

    assert false report "ok" severity note;
  end proc;
  
begin
  process
   variable v : rec_4;
  begin
    proc (v);
    wait;
  end process;
end behav;
