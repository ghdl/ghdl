entity ent is
end ent;
use work.ent.ent; -- ***this second .ent causes the crash***
architecture foo of ent is
begin
end foo;

