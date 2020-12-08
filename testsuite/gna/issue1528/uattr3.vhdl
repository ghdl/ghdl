use work.all;
--use work.attrs_pkg.all;

entity uattr3 is
  attribute ename: string;
  attribute ename of uattr3: entity is "user attributes 3";
  attribute special: boolean;
  attribute is_clock: boolean;
  attribute sign_bit: integer;
begin
end entity uattr3;


architecture tb of uattr3 is

  attribute ename of tb: architecture is uattr3'ename & " Demonstration";
  attribute stype: integer;

  signal bv : bit_vector(7 downto 0) := "11000100";
  attribute stype of bv: signal is 2;
  attribute sign_bit of bv: signal is bv'left;
  signal b : bit;
  attribute stype of b: signal is 1;
  attribute special of b: signal is true;
  signal clk : bit;
  attribute is_clock of clk: signal is true;
  signal clk1 : bit;
  signal clk2 : bit;

  signal cnt : integer := 0;

begin

  start_up: process
  begin
--report "Object: " & tb'ename & " ... starting up ...";
    wait;
  end process start_up;

  bv <= "00001111" when cnt = 0 else
        "00011110" when cnt = 1 else
        "00111100" when cnt = 2 else
        "01111000" when cnt = 3 else
        "11110000" when cnt = 4;

  b <= '1' when bv = "00111100" else '0';

  clock: process
  begin
    while cnt < 6 loop
      wait for 1 ns;
      clk <= not clk;
    end loop;
    wait;
  end process clock;

  proc1: process
  begin
    if(clk'event and clk = '1' and clk'is_clock) then
      cnt <= cnt + 1;
      clk1 <= not clk1;
      report "Tick ... " & integer'image(cnt);
    end if;

    wait on clk;
  end process;

  bmon: process(b)
  begin
--report "stype of b is: " & integer'image(b'stype);
    if b = '1' and b'special then
--report "stype of bv is: " & integer'image(bv'stype);
      assert bv = "00111100" report "Error: got unexpected bv value" severity failure;
    end if;
  end process;

  bvmon: process(bv)
    variable i_v : integer;
    attribute stype of i_v: variable is 0;
  begin
    i_v := cnt;
    case bv is
      when "00001111" =>
        report ent1'ent_name;
        report ent1'ent_type;
        report "Completeness: " & integer'image(ent1'ent_stat);
        assert cnt = 0 report "Error0: got unexpected cnt value" severity failure;
      when "00011110" =>
        assert cnt = 1 report "Error1: got unexpected cnt value" severity failure;
      when "00111100" =>
        report ent2'ent_name;
        report ent2'ent_type;
        report "Completeness: " & integer'image(ent2'ent_stat);
        assert cnt = 2 report "Error2: got unexpected cnt value" severity failure;
      when "01111000" =>
        assert cnt = 3 report "Error3: got unexpected cnt value" severity failure;
      when "11110000" =>
        report ent3'ent_name;
        report ent3'ent_type;
        report "Completeness: " & integer'image(ent3'ent_stat);
        assert cnt = 4 report "Error4: got unexpected cnt value" severity failure;
      when others =>
--report "stype of i_v is: " & integer'image(i_v'stype);
        null;
    end case;

  end process;

  d1: entity work.ent1(rtl);
  d2: entity work.ent2(bhv);
  d3: entity work.ent3(rtl);

end tb;
