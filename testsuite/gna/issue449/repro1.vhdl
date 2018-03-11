entity asd is
end entity;
architecture aa of asd is
type ia is array (natural range <>) of integer;

function a return ia is
begin
return (1,2);
end function;
function a (s : integer) return ia is
begin
return (1,2);
end function;

signal ad : integer;
begin
ad <= a(1);
end architecture;
