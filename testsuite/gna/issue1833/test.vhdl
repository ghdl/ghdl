library ieee;

entity shared2 is

  type scounter is protected
    use ieee.std_logic_1164.all;    ---<<<<   test target
    impure function inc (by : integer) return integer;
    impure function get return integer;
  end protected;

  type scounter is protected body
    variable cnt : integer := 0;

    impure function inc (by : integer) return integer is
    begin
      cnt := cnt + by;
      return cnt;
    end function;

    impure function get return integer is
    begin
      return cnt;
    end function;
  end protected body;

end entity shared2;

architecture rtl of shared2 is

  shared variable tcnt : scounter;
  signal clk : bit := '0';
  signal term : integer := 0;

begin

  process(clk)
    variable tv : integer;
  begin
    if clk'event and clk = '1' then
      tv := tcnt.inc(1);
    end if;
  end process;


  process(clk)
    variable tv : integer;
  begin
    if clk'event and clk = '0' then
      tv := tcnt.inc(2);
    end if;
  end process;


  process
  begin
    clk <= not clk;
    wait for 1 ns;
  end process;

  process
  begin
    if clk'event and clk = '0' then
      if term >= 20 then
        report "Sim end ...."; -- severity failure;
        std.env.finish;
      end if;
      term <= term + 1;
      report integer'image(tcnt.get);
    end if;
    wait on clk;
  end process;


end rtl;
