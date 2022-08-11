library ieee;
use ieee.std_logic_1164.all;

package pkg is

  type sulv_vector is array (natural range <>) of std_ulogic_vector;

  subtype NULL_RANGE is natural range 0 downto 1;
  constant NULL_SULV : std_ulogic_vector(NULL_RANGE) := (others => '0');
  constant NULL_SULV_VECTOR : sulv_vector(NULL_RANGE)(NULL_RANGE) := (others => NULL_SULV);

  function repeat(
    val : std_ulogic;
    n   : natural
  ) return std_ulogic_vector;

  function repeat(
    val : std_ulogic_vector;
    m   : natural
  ) return sulv_vector;

  function align_left(
    vec : std_ulogic_vector;
    len : positive;
    pad : std_ulogic := '0'
  ) return std_ulogic_vector;

end package;

package body pkg is

  function repeat(
    val : std_ulogic;
    n   : natural)
    return std_ulogic_vector
  is
  begin
    if n = 0 then
      return NULL_SULV;
    else
      return (n - 1 downto 0 => val);
    end if;
  end function;

  function repeat(
    val : std_ulogic_vector;
    m   : natural
  ) return sulv_vector
  is
  begin
    if m = 0 then
      return NULL_SULV_VECTOR;
    else
      return (m - 1 downto 0 => val);
    end if;
  end function;

--  function repeat(
--    val : std_ulogic_vector;
--    m   : natural
--  ) return sulv_vector
--  is
--    constant result : sulv_vector(m downto 1)(val'range) := (others => val);
--  begin
--    return result;
--  end function;

  function align_left(
    vec : std_ulogic_vector;
    len : positive;
    pad : std_ulogic := '0')
    return std_ulogic_vector
  is
    constant diff : integer := len - vec'length;
    alias v : std_ulogic_vector(vec'length - 1 downto 0) is vec;
    variable result : std_ulogic_vector(len - 1 downto 0);
  begin
    assert diff >= 0
    report "align_left: cannot align larger vector into smaller vector"
    severity failure;
    result := (v, repeat(pad, diff));
    return result;
  end function;

end package body;
