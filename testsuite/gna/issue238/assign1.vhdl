entity assign1 is
end;

use work.pkg.all;

architecture behav of assign1 is
begin
  process
   variable v : rec_4;
  begin
    v.a := 5;
    assert v.a = 5 severity failure;

    v.s := "Good";
    assert v.a = 5 severity failure;
    assert v.s = "Good" severity failure;

    assert false report "ok" severity note;
    wait;
  end process;
end behav;
