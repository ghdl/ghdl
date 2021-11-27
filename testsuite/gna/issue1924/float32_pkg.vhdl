library ieee;

package Package_Float32 is new ieee.float_generic_pkg
  generic map (
    FLOAT_EXPONENT_WIDTH => 8,    -- float32'high
    FLOAT_FRACTION_WIDTH => 23,   -- -float32'low
    FLOAT_ROUND_STYLE    => ieee.fixed_float_types.ROUND_ZERO,  -- Truncate
    FLOAT_DENORMALIZE    => false,  -- Use IEEE extended floating
    FLOAT_CHECK_ERROR    => false,  -- Turn on NAN and overflow processing
    FLOAT_GUARD_BITS     => 0,     -- number of guard bits
    NO_WARNING           => false, -- show warnings
    FIXED_PKG            => work.Package_Fixed
    );
