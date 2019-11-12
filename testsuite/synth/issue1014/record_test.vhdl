library ieee;
  use ieee.std_logic_1164.all;

entity record_test is
  port (
    o : out integer
  );
end record_test;

architecture rtl of record_test is
  type t_record is record
    int  : integer;
  end record t_record;

  function get_constants(choice : std_logic) return t_record is
    variable v_const : t_record;
  begin
    if choice = '0' then
      v_const := (int => 27.777 us / 83.333 ns);
    elsif choice = '1' then
      v_const := (int => 26.316 us / 83.333 ns);
    end if;
    return v_const;
  end function get_constants;

  constant rec_constant : t_record := get_constants('0');
  signal int_test : integer range 0 to rec_constant.int := 0;
  signal slv_test : std_logic_vector(rec_constant.int downto 0) := (others => '0');
begin
  o <= rec_constant.int;
end rtl;
