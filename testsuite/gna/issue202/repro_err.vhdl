library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library OSVVM;

entity e is
end entity;

architecture a of e is
  subtype T_DATA		is std_logic_vector(31 downto 0);
  type T_DATA_VECTOR		is array(natural range <>) of T_DATA;

  type T_SCOREBOARD_DATA is record
    IsKey : std_logic;
    Meta  : std_logic_vector(31 downto 0);
    Data  : T_DATA_VECTOR(15 downto 0);
  end record;

  function match(expected : T_SCOREBOARD_DATA; actual : T_SCOREBOARD_DATA) return boolean is
  begin
    return TRUE;
  end function;
	
  function to_string(vector : T_SCOREBOARD_DATA) return string is
  begin
    return "to_string";
  end function;

  package P_Scoreboard is new OSVVM.ScoreboardGenericPkg
    generic map (
      ExpectedType        => T_SCOREBOARD_DATA,
      ActualType          => T_SCOREBOARD_DATA,
      Match               => match,
      expected_to_string  => to_string,
      actual_to_string    => to_string
    );
  alias T_SCOREBOARD is P_Scoreboard.ScoreBoardPType

  shared variable ScoreBoard : T_SCOREBOARD;  -- this causes the error message
begin
  process
    variable v : t_scoreboard_data;
  begin
    ScoreBoard.Push(v);
    wait;
  end process;
end architecture;
