entity e3 is
end;

architecture a of e3 is
    function outer(arg : integer) return integer is
        function inner1(arg : integer) return integer is
          function inner2(arg : integer) return integer is
          begin
            return arg + 1;
          end;
        begin
            return inner2(0);
        end;
    begin
        return inner1(0);
    end;
begin
end;
