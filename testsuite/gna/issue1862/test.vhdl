library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity test;

architecture beh of test is
   type     t_slv_array  is array (natural range <>) of std_logic_vector;
   subtype  t_slv_array2  is t_slv_array(open)(15 downto 0);
begin

  process
    variable v_val_out : t_slv_array2(0 to 0);

    procedure test_proc2(test_val : out t_slv_array2) is
        variable v_val : t_slv_array2(0 to 2) := (others => (others => '0'));
    begin
        test_val := v_val(0 to test_val'length - 1);
    end procedure;

    procedure test_proc(test_val : out t_slv_array2) is
        variable v_val : t_slv_array2(0 to 0);
    begin
        test_proc2(test_val(0 to 0));
        --test_proc2(v_val);
        --test_val := v_val;
    end procedure;
  begin
    test_proc(v_val_out);
    assert false report "This should be printed" severity note;
    wait;
  end process;

end architecture beh;
