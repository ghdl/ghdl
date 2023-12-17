package abc is
  type Arr_t is array (natural range <>) of integer;
  type Rec_t is record
    Data : Arr_t;
  end record;
end package;

use work.abc;

entity test_entity is
  port
  (
    in_abc  : in abc.Rec_t;
    out_abc : out abc.Rec_t
  );
end;

use work.abc;

architecture rtl of test_entity is
  function CrashFunct(
    res   : abc.Rec_t;
    input : abc.Rec_t;
    cnt   : natural
  )
    return abc.Rec_t is
    constant SLICE : res.Data'subtype := input.Data((cnt + 1) * 4 - 1 downto cnt * 4);
  begin
    report "SLICE High: " & integer'image(SLICE'high) & " Low: " & integer'image(SLICE'low);
    -- GHDL CRASH: These crash GHDL:
    --report "input.Data'subtype High: " & integer'image(input.Data'subtype'high) & " Low: " & integer'image(input.Data'subtype'low);
    --report "res.Data'subtype High: " & integer'image(res.Data'subtype'high) & " Low: " & integer'image(res.Data'subtype'low);

    assert SLICE'high = 3 report "GHDL Bug, expected 3 in all cases" severity failure;
    assert SLICE'low = 0 report "GHDL Bug, expected 0 in all cases" severity failure;

    return res;
  end function CrashFunct;
begin
  process is
    variable some_abc : in_abc'subtype;
  begin
    out_abc <= CrashFunct(out_abc, some_abc, 0); -- Works
    out_abc <= CrashFunct(out_abc, some_abc, 1); -- GHDL BUG: Fails

    report "done";
    wait;
  end process;
end architecture;
use work.abc;

entity ent is
end entity ent;

architecture sim of ent is
  signal in_abc  : abc.Rec_t(Data(7 downto 0));
  signal out_abc : abc.Rec_t(Data(3 downto 0));
begin
  test_entity_inst : entity work.test_entity
    port map
    (
      in_abc  => in_abc,
      out_abc => out_abc
    );
end architecture sim;
