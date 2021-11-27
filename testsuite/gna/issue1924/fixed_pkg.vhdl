library ieee;

package Package_Fixed is new ieee.fixed_generic_pkg
  generic map (
    FIXED_ROUND_STYLE    => ieee.fixed_float_types.FIXED_TRUNCATE,
    FIXED_OVERFLOW_STYLE => ieee.fixed_float_types.FIXED_SATURATE,
    FIXED_GUARD_BITS     => 0,
    NO_WARNING           => false
    );
