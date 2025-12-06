entity tb_nor01 is
end;

architecture behav of tb_nor01 is
  signal l, r  : bit;
  signal res : bit;
begin
  max01_1: entity work.nor01
    port map (
      a  => l,
      b  => r,
      o => res);

  process
  begin
    for a in bit loop
      for b in bit loop
        l <= a;
        r <= b;
        wait for 1 ns;
        assert res = (a nor b) severity failure;
      end loop;
    end loop;
    wait;
  end process;
end behav;
