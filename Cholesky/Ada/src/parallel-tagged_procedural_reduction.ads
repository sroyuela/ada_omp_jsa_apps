--  This package may be used for parallelism for user defined tagged types.
--  This eliminates the need to instantiate generics to specify reducing
--  functions and Identity values.

package Parallel.Tagged_Procedural_Reduction is

   type Procedural_Result is interface;

   procedure Reducer
     (Left, Right : in out Procedural_Result)
      is abstract;
   --  Reducing operation used to compute final result. The operation
   --  needs to take two values and reduce into a single value (the Left)
   --  parameter. The Right parameter is in out to allow for finalization
   --  if necessary.

   function Identity_Value return Procedural_Result
   is abstract;
   --  A special value that when applied as the right operand of the
   --  Reducing function, does not change the value of the reducing result.

end Parallel.Tagged_Procedural_Reduction;
