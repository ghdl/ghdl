entity tb_nand01 is
end;

architecture behav of tb_nand01 is
  signal l, r  : bit;
  signal res : bit;
begin
  max01_1: entity work.nand01
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
        assert res = (a nand b) severity failure;
      end loop;
    end loop;
    wait;
  end process;
end behav;
