with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure Pattr is
   Xdigit : constant array (0 .. 15) of Character := "0123456789abcdef";

   procedure Disp_Lit (Z : Natural; Known : Boolean; S : String) is
   begin
      Put_Line (S);
   end Disp_Lit;

   procedure Disp_Float_Lit
     (Lit_Type : Natural; Known : Boolean; Val : IEEE_Float_64)
   is
      pragma Assert (IEEE_Float_64'Machine_Radix = 2);
      pragma Assert (IEEE_Float_64'Machine_Mantissa = 53);
      Exp : Integer;
      Man : Unsigned_64;
      --  Res: sign(1) + 0x(2) + Man(53 / 3 ~= 18) + p(1) + sing(1) + exp(4)
      Str : String (1 .. 1 + 2 + 18 + 1 + 1 + 4);
      P : Natural;
      Neg : Boolean;
   begin
      Exp := IEEE_Float_64'Exponent (Val) - 1;
      Man := Unsigned_64 (abs (IEEE_Float_64'Fraction (Val)) * 2.0 ** 53);

      --  Use decimal representation if there is no digit after the dot.
      if Man = 0 then
         Disp_Lit (Lit_Type, Known, "0.0");
      else
         pragma Assert (Shift_Right (Man, 52) = 1);

         --  Remove hidden 1.
         Man := Man and (2**52 - 1);

         --  Remove trailing hex 0.
         while Man /= 0 and (Man rem 16) = 0 loop
            Man := Man / 16;
         end loop;

         --  Exponent.
         P := Str'Last;
         if Exp < 0 then
            Neg := True;
            Exp := -Exp;
         else
            Neg := False;
         end if;
         loop
            Str (P) := Xdigit (Exp rem 10);
            P := P - 1;
            Exp := Exp / 10;
            exit when Exp = 0;
         end loop;
         if Neg then
            Str (P) := '-';
            P := P - 1;
         end if;
         Str (P) := 'p';
         P := P - 1;

         --  Mantissa.
         loop
            Str (P) := Xdigit (Natural (Man and 15));
            P := P - 1;
            Man := Man / 16;
            exit when Man = 0;
         end loop;

         P := P - 4;
         Str (P + 1) := '0';
         Str (P + 2) := 'x';
         Str (P + 3) := '1';
         Str (P + 4) := '.';

         if Val < 0.0 then
            Str (P) := '-';
            P := P - 1;
         end if;

         Disp_Lit (Lit_Type, Known, Str (P + 1 .. Str'Last));
      end if;
   end Disp_Float_Lit;

  subtype T is IEEE_Float_64;
  V : T;
begin
  if Argument_Count /= 1 then
    Put_Line ("usage : pattr FNUM");
    return;
  end if;

  V := T'Value (Argument (1));

  Put_Line ("Machine Radix:" & Natural'Image (T'Machine_Radix));
  Put_Line ("Machine Mantissa:" & Natural'Image (T'Machine_Mantissa));
  Put_Line ("Machine Emin: " & Natural'Image (T'Machine_Emin));
  Put_Line ("Machine Emax: " & Natural'Image (T'Machine_Emax));
  Put_Line ("Exponent: " & Integer'Image (T'Exponent (V)));
  Put_Line ("Fraction: " & T'Image (T'Fraction (V)));
  Disp_Float_Lit (1, False, V);
end pattr;
