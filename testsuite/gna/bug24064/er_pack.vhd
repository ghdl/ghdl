--------------------------------------------------------------------------------
--! @file
--! @brief A bunch of useful functions for (non)synthesizable  VHDL
--------------------------------------------------------------------------------
library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    use ieee.math_real.all;
library std;
    use std.textio.all;

package er_pack is
    -- constants used inside function 'rt'
    constant simres  : time := 1 ps;    -- simulation resolution (time)
    constant resreal : real := 1.0e-12; -- simulation resolution (real)

    ----------------------------------------------------------------------------
    -- Types, Subtypes, and constants
    ----------------------------------------------------------------------------
    type integer_vector  is array (integer range <>) of integer;
    type natural_vector  is array (integer range <>) of natural;
    type real_vector     is array (integer range <>) of real;
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- synthesis off
    function print_nibble(arg : std_logic_vector(3 downto 0)) return character;
    function print_message(arg : string) return boolean;
    -- synthesis on

    ----------------------------------------------------------------------------
    -- print function
    ----------------------------------------------------------------------------
    -- synthesis off
    function slv2string (arg : in std_logic_vector) return string;
    -- synthesis on

    ----------------------------------------------------------------------------
    --! Return a vector of all ones.
    --! @param arg The number of bits in the output vector.
    --! @returns A vector of all ones.
    ----------------------------------------------------------------------------
    function ones (arg : natural) return std_logic_vector;

    ----------------------------------------------------------------------------
    --! Return a vector of all zeros.
    --! @param arg The number of bits in the output vector.
    --! @returns A vector of all zeros.
    ----------------------------------------------------------------------------
    function zeros(arg : natural) return std_logic_vector;

    ----------------------------------------------------------------------------
    --! Return the maximum (max positive) 2's complement value that can be
    --! expressed in the given number of bits.  This is defined as {0,1,...,1}.
    --! @param arg The number of bits in the output vector.
    --! @returns The maximum 2's complement value.
    ----------------------------------------------------------------------------
    function max  (arg : natural) return std_logic_vector;

    ----------------------------------------------------------------------------
    --! Return the minimum (max negative) 2's complement value that can be
    --! expressed in the given number of bits.  This is defined as {1,0,...,0}.
    --! @param arg The number of bits in the output vector.
    --! @returns The minimum 2's complement value.
    ----------------------------------------------------------------------------
    function min  (arg : natural) return std_logic_vector;

    ----------------------------------------------------------------------------
    --! Return the maximum value of two input values.
    --! @param a The first input value
    --! @param b The second input value
    --! @returns The maximum value of a and b
    ----------------------------------------------------------------------------
    function max(a:natural; b:natural) return natural;

    ----------------------------------------------------------------------------
    --! Return the minimum value of two input values.
    --! @param a The first input value
    --! @param b The second input value
    --! @returns The minimum value of a and b
    ----------------------------------------------------------------------------
    function min(a:natural; b:natural) return natural;

    ----------------------------------------------------------------------------
    --! Return the next multiple of the given variable
    --! @param arg The input value
    --! @param mult The multiple
    --! @returns arg rounded up to the next multiple of mult
    ----------------------------------------------------------------------------
    function next_multiple (arg : natural; mult : natural) return natural;

    ----------------------------------------------------------------------------
    --! Log function
    --! This might be the single most useful function in all of VHDL.  It simply
    --! returns the log of a value
    --! @param base The base to use for the log.
    --! @param arg The value to log.
    --! @returns The log (arg)
    ----------------------------------------------------------------------------
    function log (base : positive; arg : positive) return natural;

    ----------------------------------------------------------------------------
    --! Log2 function
    --! This might be the single most useful function in all of VHDL.  It simply
    --! returns the log2 of a value
    --! @param arg The value to log.
    --! @returns The log2 (arg)
    ----------------------------------------------------------------------------
    function log2 (arg : positive) return natural;

    ----------------------------------------------------------------------------
    --! Number of Bits function
    --! Return the number of bits necessary to hold a particular values.  This
    --! is the log2 function rounded up
    --! @param arg The value to store
    --! @returns The number of bits necessary to hold arg
    ----------------------------------------------------------------------------
    function num_bits (arg : positive) return natural;

    ----------------------------------------------------------------------------
    --! delay via register
    --! This function should take place in a clocked process, but is an easy
    --! way to delay a signal
    --! @param reg The shift register that is doing the delaying
    --! @param sig The signal that is being delayed.  This is put in the low
    --!        address of the signal.
    --! @returns This will return the shifted (delayed) vector
    ----------------------------------------------------------------------------
    function delay (
        reg : natural_vector;
        sig : natural) return natural_vector;

    ----------------------------------------------------------------------------
    --! delay via register
    --! This function should take place in a clocked process, but is an easy
    --! way to delay a signal
    --! @param reg The shift register that is doing the delaying
    --! @param sig The signal that is being delayed.  This is put in the low
    --!        address of the signal.
    --! @returns This will return the shifted (delayed) vector
    ----------------------------------------------------------------------------
    function delay (
        reg : integer_vector;
        sig : integer) return integer_vector;

    ----------------------------------------------------------------------------
    --! delay via register
    --! This function should take place in a clocked process, but is an easy
    --! way to delay a signal
    --! @param reg The shift register that is doing the delaying
    --! @param sig The signal that is being delayed.  This is put in the low
    --!        address of the signal.
    --! @returns This will return the shifted (delayed) vector
    ----------------------------------------------------------------------------
    function delay (
        reg : std_logic_vector;
        sig : std_logic) return std_logic_vector;

    ----------------------------------------------------------------------------
    --! Return a std_logic that is the result of a rising edge detector.  There
    --! will need to be an input register with at least two values in it, since
    --! different indexes are used to derive the output.
    --! @param reg The input shift/Delay register
    --! @param idx (Optional) The index of the input reg register to start the
    --!        the detection.  The default value is the highest most index.
    --! @return not reg(idx) and reg(idx-1);
    ----------------------------------------------------------------------------
    function rising_edge (reg : std_logic_vector; idx : integer) return std_logic;
    function rising_edge (reg : std_logic_vector)                return std_logic;

    ----------------------------------------------------------------------------
    --! Return a std_logic that is the result of a falling edge detector.  There
    --! will need to be an input register with at least two values in it, since
    --! different indexes are used to derive the output.
    --! @param reg The input shift/Delay register
    --! @param idx (Optional) The index of the input reg register to start the
    --!        the detection.  The default value is the highest most index.
    --! @return reg(idx) and not reg(idx-1);
    ----------------------------------------------------------------------------
    function falling_edge (reg : std_logic_vector; idx : integer) return std_logic;
    function falling_edge (reg : std_logic_vector)                return std_logic;

    ----------------------------------------------------------------------------
    --! Return a std_logic that is the result of an edge detector.  There will
    --! need to be an input register with at least two values in it, since
    --! different indexes are used to derive the output.
    --! @param reg The input shift/Delay register
    --! @param idx (Optional) The index of the input reg register to start the
    --!        the detection.  The default value is the highest most index.
    --! @return reg(idx) xor reg(idx-1);
    ----------------------------------------------------------------------------
    function edge (reg : std_logic_vector)                return std_logic;
    function edge (reg : std_logic_vector; idx : integer) return std_logic;

    ----------------------------------------------------------------------------
    --! Flip a register.  This will put the high bits in the low positions,
    --! producing a mirror image of the bits.  It will preserve the range of the
    --! input vector.
    --! @param ret The input register.
    --! @return The flipped version of the input register.
    ----------------------------------------------------------------------------
    function flip (reg : std_logic_vector) return std_logic_vector;

    ----------------------------------------------------------------------------
    --! Convert a real number to a std_logic_vector with a given number of bits.
    --! The input real should be a value between 1.0 and -1.0.  Any value
    --! outside of this range will saturate the output vector.
    --! @param l The real number
    --! @param b The number of bits for the std_logic_vector
    ----------------------------------------------------------------------------
    function to_slv(l:real; b:natural) return std_logic_vector;

    ----------------------------------------------------------------------------
    -- Convert a time to a real representation for the number of seconds
    -- @param t The time to convert
    -- @return The real time (in seconds)
    ----------------------------------------------------------------------------
    function rt(t : time) return real;

    ----------------------------------------------------------------------------
    --! Divide two times.  Return a real
    --! @param l The numerator
    --! @param r The denominator
    --! @returns The result of the divide in a real number
    ----------------------------------------------------------------------------
    function "/" (l, r : time) return real;

    ----------------------------------------------------------------------------
    --! Priority decoder
    --! Return the lowest index that is set high.
    --! @param reg the register to decode
    --! @returns The index of the highest bit set.  If the whole register is
    --!     zero, then it returns an out-of-bound integer
    ----------------------------------------------------------------------------
    function priority_decode(reg : std_logic_vector) return integer;

    ----------------------------------------------------------------------------
    --! Saturate an unsigned value to the given number of bits.
    --! @param val The unsigned value
    --! @param bits The number of bits
    --! @returns If the input value is greater than the requested number of bits
    --!    can hold, it will return 2^bits-1.  All other cases will return the
    --!    original number.
    ----------------------------------------------------------------------------
    function saturate(val : unsigned; bits : natural) return unsigned;
    function usat(val : std_logic_vector; bits : natural ) return std_logic_vector;

    ----------------------------------------------------------------------------
    --! Saturate a signed value to the given number of bits.
    --! @param val The signed value
    --! @param bits The number of bits
    --! @returns If the absolute value of the input value is greater than
    --! 2^(bits-1)-1, then return the appriate signed version of 2^(bits-1)-1.
    --!    All other cases will return the original number.
    ----------------------------------------------------------------------------
    function saturate(val : signed; bits : natural) return signed;
    function ssat(val : std_logic_vector; bits : natural ) return std_logic_vector;

    ----------------------------------------------------------------------------
    --! numeric_std helper functions
    --! (un)signed shift left/right
    ----------------------------------------------------------------------------
    --! unsigned shift left
    function usl (val : std_logic_vector; bits : natural) return std_logic_vector;
    --! unsigned shift right
    function usr (val : std_logic_vector; bits : natural) return std_logic_vector;
    --! signed shift left
    function ssl (val : std_logic_vector; bits : natural) return std_logic_vector;
    --! signed shift right
    function ssr (val : std_logic_vector; bits : natural) return std_logic_vector;

end package er_pack;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Package body
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
package body er_pack is
    ----------------------------------------------------------------------------
    -- synthesis off
    function print_nibble(arg : std_logic_vector(3 downto 0))
    return character is
        variable ret : character;
        variable num : natural;
        -- status variables
        variable is_x    : boolean;
        variable is_u    : boolean;
        variable is_dash : boolean;
        variable is_z    : boolean;
        variable is_w    : boolean;
    begin
        for idx in arg'range loop
            -- take care of the special cases
            case arg(idx) is
                when 'X' => is_x    := true;
                when 'U' => is_u    := true;
                when '-' => is_dash := true;
                when 'Z' => is_z    := true;
                when 'W' => is_w    := true;
                when others => NULL;
            end case;
        end loop;

        -- Print it
        if    is_x    then ret := 'X';
        elsif is_u    then ret := 'U';
        elsif is_dash then ret := '-';
        elsif is_z    then ret := 'Z';
        elsif is_w    then ret := 'W';
        else
            num := to_integer(unsigned(arg));
            case num is
                when 15 =>     ret := 'F';
                when 14 =>     ret := 'E';
                when 13 =>     ret := 'D';
                when 12 =>     ret := 'C';
                when 11 =>     ret := 'B';
                when 10 =>     ret := 'A';
                when  9 =>     ret := '9';
                when  8 =>     ret := '8';
                when  7 =>     ret := '7';
                when  6 =>     ret := '6';
                when  5 =>     ret := '5';
                when  4 =>     ret := '4';
                when  3 =>     ret := '3';
                when  2 =>     ret := '2';
                when  1 =>     ret := '1';
                when  0 =>     ret := '0';
                when others => ret := 'J';
            end case;
        end if;

        -- we're done
        return ret;
    end function print_nibble;

    --! Just print a string.  It's not hard, but it takes more than one line
    --! without the function
    function print_message(arg : string) return boolean is
        variable out_line  : line;
    begin
        write(out_line, arg);
        writeline(output, out_line);
        return true;
    end function print_message;

    -- print function
    function slv2string (arg : in std_logic_vector) return string is
        variable ret : string (1 to arg'length/4+1);
        variable jdx : integer;
        variable tmp_nibble : std_logic_vector(3 downto 0);
        variable kdx : natural := 1;
    begin
        -- Try to get a useful hex value
        jdx := 0;
        kdx := ret'high;
        for idx in arg'reverse_range loop
            -- fill the next value of the nibble
            tmp_nibble(jdx) := arg(idx);

            -- correct jdx and print accordingly
            if jdx = 3 then
                -- reset the jdx value
                jdx      := 0;
                ret(kdx) := print_nibble(tmp_nibble);

                -- correct kdx
                kdx := kdx - 1;
            else
                -- decrement jdx
                jdx := jdx + 1;
            end if;

        end loop;

        -- edge cases
        if jdx /= 0 then
            tmp_nibble(3 downto jdx) := (others => '0');
            ret(kdx)                 := print_nibble(tmp_nibble);
            return ret;
        end if;

        -- if we got here, then we have an exact number of nibbles.  Give back
        -- all but one character.
        return ret(2 to ret'high);
    end function slv2string;
    -- synthesis on

    ----------------------------------------------------------------------------
    function ones (arg : natural) return std_logic_vector is
        variable ret : std_logic_vector(arg-1 downto 0) := (others => '1');
    begin
        return ret;
    end function ones;
    ----------------------------------------------------------------------------
    function zeros(arg : natural) return std_logic_vector is
        variable ret : std_logic_vector(arg-1 downto 0) := (others => '0');
    begin
        return ret;
    end function zeros;
    ----------------------------------------------------------------------------
    function max  (arg : natural) return std_logic_vector is
        variable ret : std_logic_vector(arg-1 downto 0) := '0' & ones(arg-1);
    begin
        return ret;
    end function max;
    ----------------------------------------------------------------------------
    function min  (arg : natural) return std_logic_vector is
        variable ret : std_logic_vector(arg-1 downto 0) := '1' & zeros(arg-1);
    begin
        return ret;
    end function min;
    ----------------------------------------------------------------------------
    function max(a:natural; b:natural) return natural is
    begin
        if a > b then
            return a;
        end if;
        return b;
    end function max;
    ----------------------------------------------------------------------------
    function min(a:natural; b:natural) return natural is
    begin
        if a < b then
            return a;
        end if;
        return b;
    end function min;
    ----------------------------------------------------------------------------
    function next_multiple (arg : natural; mult : natural)
    return natural is
    begin
        return (arg / mult) * mult;
    end function next_multiple;

    ----------------------------------------------------------------------------
    function log (base : positive; arg : positive) return natural is
        variable div : positive := arg;
        variable ret  : natural  := 0;
    begin
        while div > 1 loop
            div := div / base;
            ret := ret + 1;
        end loop;
        return ret;
    end function log;

    ----------------------------------------------------------------------------
    function log2 (arg : positive) return natural is
    begin
        return log(2, arg);
    end function log2;

    ----------------------------------------------------------------------------
    function num_bits (arg : positive) return natural is
        variable ret : natural := log2(arg);
    begin
        if 2**ret /= arg then
            ret := ret + 1;
        end if;
        return ret;
    end function num_bits;

    ----------------------------------------------------------------------------
    function delay (
        reg : integer_vector;
        sig : integer)
    return integer_vector is
        variable ret : integer_vector(reg'range);
    begin
        if ret'ascending then
            ret := sig & reg(reg'low to reg'high-1);
        else
            ret := reg(reg'high-1 downto reg'low) & sig;
        end if;
        return ret;
    end function;

    function delay (
        reg : natural_vector;
        sig : natural)
    return natural_vector is
        variable ret : natural_vector(reg'range);
    begin
        if ret'ascending then
            ret := sig & reg(reg'low to reg'high-1);
        else
            ret := reg(reg'high-1 downto reg'low) & sig;
        end if;
        return ret;
    end function;

    function delay (
        reg : std_logic_vector;
        sig : std_logic)
    return std_logic_vector is
        variable ret : std_logic_vector(reg'range);
    begin
        if ret'ascending then
            ret := sig & reg(reg'low to reg'high-1);
        else
            ret := reg(reg'high-1 downto reg'low) & sig;
        end if;
        return ret;
    end function;

    ----------------------------------------------------------------------------
    function rising_edge (
        reg : std_logic_vector)
    return std_logic is
        variable idx : integer := reg'high;
    begin
        return rising_edge(reg, idx);
    end function;
    ----------------------------------------------------------------------------
    function rising_edge (
        reg : std_logic_vector;
        idx : integer)
    return std_logic is
    begin
        -- Check the input for validity
        assert reg'length >= 2
            report "input vector not long enough" severity error;
        assert idx <= reg'high and idx > reg'low
            report "input vector not long enough" severity error;

        -- now just return the answer
        return not reg(idx) and reg(idx-1);
    end function;
    ----------------------------------------------------------------------------
    function falling_edge (
        reg : std_logic_vector)
    return std_logic is
        variable idx : integer := reg'high;
    begin
        return falling_edge(reg, idx);
    end function falling_edge;
    ----------------------------------------------------------------------------
    function falling_edge (
        reg : std_logic_vector;
        idx : integer)
    return std_logic is
    begin
        -- Check the input for validity
        assert reg'length >= 2
            report "input vector not long enough" severity error;
        assert idx <= reg'high and idx > reg'low
            report "input vector not long enough" severity error;

        -- now just return the answer
        return reg(idx) and not reg(idx-1);
    end function falling_edge;
    ----------------------------------------------------------------------------
    function edge (
        reg : std_logic_vector)
    return std_logic is
        variable idx : integer := reg'high;
    begin
        return edge(reg, idx);
    end function edge;
    ----------------------------------------------------------------------------
    function edge (
        reg : std_logic_vector;
        idx : integer)
    return std_logic is
    begin
        -- Check the input for validity
        assert reg'length >= 2
            report "input vector not long enough" severity error;
        assert idx <= reg'high and idx > reg'low
            report "input vector not long enough" severity error;

        -- now just return the answer
        return reg(idx) xor reg(idx-1);
    end function edge;
    ----------------------------------------------------------------------------
    function flip (reg : std_logic_vector) return std_logic_vector is
        variable ret : std_logic_vector(reg'range);
        variable idx : integer := reg'high;
        variable jdx : integer := reg'low;
    begin
        while jdx < idx loop
            -- Populate ret with the reg bits backwards
            ret(idx) := reg(jdx);
            ret(jdx) := reg(idx);

            -- update the counters
            idx := idx + 1;
            jdx := jdx + 1;
        end loop;

        -- return the flipped register
        return ret;
    end function flip;

    ----------------------------------------------------------------------------
    function to_slv(l:real; b:natural) return std_logic_vector is
        variable slv    : std_logic_vector(b-1 downto 0);
        variable temp_r : real;
        variable temp_i : integer;
    begin
        -- Check the bounds and saturate when necessary
        if l <= -1.0 then
            slv := min(b);
        elsif l >= 1.0 then
            slv := max(b);
        else
            -- Compute the answer
            temp_r := l * real(2**(b-1)-1);   -- Scale the real to not overflow
            temp_i := integer(round(temp_r)); -- round it and turn it into an integer
            slv    := std_logic_vector(to_signed(temp_i, b)); -- Turn it to an slv
        end if;

        -- Now just return it
        return slv;
    end function to_slv;

    ----------------------------------------------------------------------------
    function rt(t : time) return real is
        variable nat_time  : natural := t / simres;
        variable real_time : real    := real(nat_time);
    begin
        return real_time * resreal;
    end;

    ----------------------------------------------------------------------------
    function "/" (l, r : time) return real is
        variable real_l : real := rt(l);
        variable real_r : real := rt(r);
    begin
        return real_l / real_r;
    end function "/";

    ----------------------------------------------------------------------------
    function priority_decode(reg : std_logic_vector) return integer is
        variable ret : integer;
    begin
        -- Start with the default value
        if reg'ascending then
            ret := reg'right + 1;
        else
            ret := reg'right - 1;
        end if;

        -- now determine which one is lit
        for idx in reg'reverse_range loop
            if reg(idx) = '1' then
                ret := idx;
            end if;
        end loop;

        -- return it
        return ret;
    end function priority_decode;

    ----------------------------------------------------------------------------
    function saturate(val : unsigned; bits : natural) return unsigned is
        variable max_val : unsigned(bits-1 downto 0) := unsigned(ones(bits));
    begin
        -- Check the value over the max
        if val > max_val then
            return resize(max_val, val'length);
        end if;

        -- If we got here, we just return the value
        return val;
    end function saturate;

    -- The std_logic_vector version
    function usat(val : std_logic_vector; bits : natural ) return std_logic_vector is
    begin
        return std_logic_vector(saturate(unsigned(val), bits));
    end function usat;

    ----------------------------------------------------------------------------
    function saturate(val : signed; bits : natural) return signed is
        variable max_val : signed(bits-1 downto 0) := '0' & signed(ones (bits-2)) & '1';
        variable min_val : signed(bits-1 downto 0) := '1' & signed(zeros(bits-2)) & '1';
    begin
        -- Check the value over the max
        if val > max_val then
            return resize(max_val, val'length);
        elsif val < min_val then
            return resize(min_val, val'length);
        end if;

        -- If we got here, we just return the value
        return val;
    end function saturate;

    -- The std_logic_vector version
    function ssat(val : std_logic_vector; bits : natural ) return std_logic_vector is
    begin
        return std_logic_vector(saturate(signed(val), bits));
    end function ssat;

    ----------------------------------------------------------------------------
    -- numeric_std helper functions
    ----------------------------------------------------------------------------
    function usl (val : std_logic_vector; bits : natural) return std_logic_vector is
    begin
        return std_logic_vector(shift_left(unsigned(val), bits));
    end function usl;
    function usr (val : std_logic_vector; bits : natural) return std_logic_vector is
    begin
        return std_logic_vector(shift_right(unsigned(val), bits));
    end function usr;
    function ssl (val : std_logic_vector; bits : natural) return std_logic_vector is
    begin
        return std_logic_vector(shift_left(signed(val), bits));
    end function ssl;
    function ssr (val : std_logic_vector; bits : natural) return std_logic_vector is
    begin
        return std_logic_vector(shift_right(signed(val), bits));
    end function ssr;
end package body;


