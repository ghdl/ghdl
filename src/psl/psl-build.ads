with PSL.Nodes; use PSL.Nodes;

package PSL.Build is
   Optimize_Final : Boolean := True;

   function Build_FA (N : Node) return NFA;
end PSL.Build;
