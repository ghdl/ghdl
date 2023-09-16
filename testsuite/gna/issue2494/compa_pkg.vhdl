library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


package compa_pkg is

type test_rec is record
	a	: std_logic_vector;
	b	: std_logic_vector;
end record;

procedure test_record_proc (signal test_sig:out std_logic_vector);


end package;

package body compa_pkg is

procedure test_record_proc (signal test_sig :out std_logic_vector) is
variable test_sig_v	:std_logic_vector(test_sig'range);
begin
test_sig_v := (others => '1');
test_sig <= test_sig_v;

end procedure;


end package body;
