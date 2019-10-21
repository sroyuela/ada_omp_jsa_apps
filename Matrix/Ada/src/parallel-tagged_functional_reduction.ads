--  This package may be used for parallelism for user defined tagged types.
--  This eliminates the need to instantiate generics to specify reducing
--  functions and Identity values.
--  For tagged types, it is probably recommended that the Procedural_Result
--  inteface be used instead of the Functional_Result, with the thought that
--  type tagged type might be larger and too inefficient to return as a
--  function result?

package Parallel.Tagged_Functional_Reduction is

   type Functional_Result is interface;

   function Reducer
     (Left, Right : Functional_Result) return Functional_Result
      is abstract;
   --  Reducing operation used to compute final result. The operation
   --  needs to take two values and reduce into a single value (the Left)
   --  parameter. The Right parameter is in out to allow for finalization
   --  if necessary.

   function Identity_Value return Functional_Result
   is abstract;
   --  A special value that when applied as the right operand of the
   --  Reducing function, does not change the value of the reducing result.

end Parallel.Tagged_Functional_Reduction;
