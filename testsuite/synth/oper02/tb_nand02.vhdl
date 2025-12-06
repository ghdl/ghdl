entity tb_nand02 is
end;

architecture behav of tb_nand02 is
  signal l, r  : boolean;
  signal res : boolean;
begin
  max01_1: entity work.nand02
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
        assert res = (a nand b) severity failure;
      end loop;
    end loop;
    wait;
  end process;
end behav;
