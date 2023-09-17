library modulation_lib;
use modulation_lib.types_pkg.all;
package symbol_stack_pkg is new modulation_lib.generic_stack_pkg generic map (datatype => t_symbol);
