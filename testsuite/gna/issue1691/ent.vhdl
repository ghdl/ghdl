entity ent is
end entity ent;

architecture beh of ent is
  function test
	return time is
  begin
	return now;
  end function;
begin
end architecture beh;
