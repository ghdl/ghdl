use work.pkg.all;

entity riassoc03 is
  port (v : nat_arr (1 to 2);
        res : out natural);
end riassoc03;

architecture behav of riassoc03 is
begin
  process (v)
    variable t : natural;
  begin
    t := 0;
    for i in v'range loop
      t := t + v (i);
    end loop;
    res <= t;
  end process;
end behav;

entity iassoc03 is
  port (a, b : natural;
        res : out natural);
end iassoc03;

architecture behav of iassoc03 is
begin
  inst : entity work.riassoc03
    port map (v (1) => a, v (2) => b, res => res);
end behav;
