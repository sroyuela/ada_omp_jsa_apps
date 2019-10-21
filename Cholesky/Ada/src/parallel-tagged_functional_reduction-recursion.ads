generic

   type Work_Type is private;
   --  Data type to be processed recursively

package Parallel.Tagged_Functional_Reduction.Recursion is

   type Parallelism_Manager is limited interface;

   function Execute_Parallel_Subprogram
     (Dispatcher : in out Parallelism_Manager;
      Item : Work_Type;
      --  Top level item to process recursively
      Process : not null access
        function (Item : Work_Type) return Functional_Result'Class)

      return Functional_Result'Class
   is abstract;

end Parallel.Tagged_Functional_Reduction.Recursion;
