--  Generic to use for parallel subprograms that do not generate a final result

generic
   type Work_Type is private;
   --  Data type to be processed recursively
package Parallel.Recursion is

   type Parallelism_Manager is limited interface;

   procedure Execute_Parallel_Subprogram
     (Manager : in out Parallelism_Manager;

      Item : Work_Type;
      --  Top level item to process recursively

      Worker_Count : Worker_Count_Type := Default_Worker_Count;
      Process   : not null access procedure (Item : Work_Type))
   is abstract;

end Parallel.Recursion;
