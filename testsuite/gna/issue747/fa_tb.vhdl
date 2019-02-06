library ieee;
use ieee.std_logic_1164.all;

entity fa_tb is
end fa_tb;

architecture fa_behave of fa_tb is
  signal a, b, ci, co, s : std_ulogic;

begin
  DUT : entity work.fa
    port map(a  => a,
             b  => b,
             ci => ci,
             s  => s,
             co => co);

  process
    type pattern_type is record
      a, b, ci, s, co : std_ulogic;
    end record;

    type pattern_array is array (natural range <>) of pattern_type;
    constant patterns : pattern_array :=
      (('0', '0', '0', '0', '0'),
       ('0', '0', '1', '1', '0'),
       ('0', '1', '0', '1', '0'),
       ('0', '1', '1', '0', '1'),
       ('1', '0', '0', '1', '0'),
       ('1', '0', '1', '0', '1'),
       ('1', '1', '0', '0', '1'),
       ('1', '1', '1', '1', '1'));

  begin
    for i in pattern_array'range loop
      a  <= pattern_array(i).a;
      b  <= pattern_array(i).b;
      ci <= pattern_array(i).ci;

      wait for 1 ns;
      
      assert s = pattern_array(i).s
        report "bad sum value" severity error;

      assert co = pattern_array(i).co
        report "bad co value" severity error;

    end loop;

    assert false
      report "end of test" severity note;

    wait;
  end process;

end fa_behave;

