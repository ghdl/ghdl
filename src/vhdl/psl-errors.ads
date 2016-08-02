with Types; use Types;
with Errorout;
with Files_Map;

package PSL.Errors is
   function Image (Loc : Location_Type; Filename : Boolean := True)
                  return String renames Files_Map.Image;

   procedure Error_Kind (Msg : String; N : PSL_Node) renames
     Errorout.Error_Kind;

   procedure Error_Msg_Parse (Msg: String)
     renames Errorout.Error_Msg_Parse_1;
   procedure Error_Msg_Sem (Msg: String; Loc: PSL_Node)
     renames Errorout.Error_Msg_Sem_1;
end PSL.Errors;
