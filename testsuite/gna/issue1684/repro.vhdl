entity repro is
end;

architecture behav of repro is
  signal count : natural;

  procedure proc (signal cnt : inout natural;
                  signal inp : bit_vector (3 downto 0)) is
  begin
    report "proc executed";
    if inp'event then
      cnt <= cnt + 1;
    end if;
  end proc;

  signal s : bit_vector (3 downto 0);
begin
  proc (count, inp (3 downto 0) => s);

  process
  begin
    wait for 1 ns;
    s <= x"3";
    wait for 1 ns;
    s <= x"b";
    wait for 1 ns;
    assert count = 2 severity failure;
    wait;
  end process;
end behav;
