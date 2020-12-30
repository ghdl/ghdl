library ieee;
use ieee.std_logic_1164.all;

entity ent4 is
  generic (
    TUSER_WIDTH : natural := 1);
end ent4;

architecture ent of ent4 is
  type std_logic_array_t is array (natural range <>) of std_logic_vector;
  subtype tuser_array_t is std_logic_array_t(open)(TUSER_WIDTH - 1 downto 0);

  procedure write_data (constant tuser : tuser_array_t) is
  begin
    for i in tuser'range loop
      report integer'image(i) & " =>" & to_bstring(tuser(i));
    end loop;

    assert tuser(1)(0) = '1'; -- <<<<<====== This should not fail
  end procedure;

  constant data2 : tuser_array_t(0 to 1) := (0 => "0", 1 => "1");
begin
  process
  begin
    write_data(data2);
    wait;
  end process;
end ent;
