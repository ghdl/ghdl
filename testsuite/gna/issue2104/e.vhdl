entity e is
end;

architecture a of e is
    function outer(arg : integer) return integer is
        function inner(arg : integer) return integer is
        begin
            return outer(0);
        end;
    begin
        return inner(0);
    end;
begin
end;
