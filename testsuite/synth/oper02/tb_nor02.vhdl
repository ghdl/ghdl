entity tb_nor02 is
end;

architecture behav of tb_nor02 is
  signal l, r  : boolean;
  signal res : boolean;
begin
  max01_1: entity work.nor02
    port map (
      a  => l,
      b  => r,
      o => res);

  process
  begin
    for a in boolean loop
      for b in boolean loop
        l <= a;
        r <= b;
        wait for 1 ns;
        assert res = (a nor b) severity failure;
      end loop;
    end loop;
    wait;
  end process;
end behav;
