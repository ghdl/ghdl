entity ent is
end ent;

architecture a of ent is
    signal test : bit;
    alias a1 : bit is test;
    alias a2 : bit is a1;
begin
end a;
