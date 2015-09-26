entity ent is
end entity;

architecture a of ent is
  procedure proc(
    constant seq : in string;
    signal clk        : in  bit) is
  begin
    for i in seq'left to seq'right loop
      report ("left=" & integer'image(seq'left) &
              ", right=" & integer'image(seq'right) &
              ", i=" & integer'image(i));
      report "seq(i)=" & character'image(seq(i));
      wait until rising_edge(clk);
    end loop;
  end procedure;

  signal clk : bit := '0';
begin

  clk <= not clk after 1 ns;
  main : process
    variable var : string(1 to 10 - 1);
  begin
    var := (others => '0');
    proc(var & "0", clk);
    wait;
  end process;
end architecture;

