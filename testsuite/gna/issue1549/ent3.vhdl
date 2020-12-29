library ieee;
use ieee.std_logic_1164.all;

entity ent3 is
  generic (
    TUSER_WIDTH : natural := 1);
end ent3;

architecture ent of ent3 is
  type data_t is record
    tuser : std_logic_vector;
  end record;

  type data_array_t is array (natural range <>) of data_t;
  type std_logic_array_t is array (natural range <>) of std_logic_vector;
  subtype tuser_array_t is std_logic_array_t(open)(TUSER_WIDTH - 1 downto 0);

  procedure write_data (constant tuser : tuser_array_t) is
  begin
    for i in tuser'range loop
      report integer'image(i) & " =>" & to_bstring(tuser(i));
    end loop;

    assert tuser(1)(0) = '1'; -- <<<<<====== This should not fail
  end procedure;

  procedure handle_data_array ( constant data : data_array_t) is
    variable local_tuser : std_logic_array_t(data'range)(TUSER_WIDTH - 1 downto 0);
  begin
    -- Convert a list of tuples into two lists
    for i in data'range loop
      local_tuser(i) := data(i).tuser;
      assert local_tuser(i) = data(i).tuser
        report "local_tuser and data(i).tuser should not be different here";
    end loop;
    report "local(0)=" & to_bstring(local_tuser(0));
    report "local(1)=" & to_bstring(local_tuser(1));

    write_data(tuser => local_tuser);
  end;

  constant data : data_array_t(0 to 1)(tuser(0 downto 0)) :=
    (0 => (tuser => (0 downto 0 => '0')),
     1 => (tuser => (0 downto 0 => '1')));

  constant data2 : tuser_array_t(0 to 1) := (0 => "0", 1 => "1");
begin
  process
  begin
    write_data(data2);
    report "data(0).tuser=" & to_bstring(data(0).tuser);
    report "data(1).tuser=" & to_bstring(data(1).tuser);
    handle_data_array(data);
    wait;
  end process;
end ent;
