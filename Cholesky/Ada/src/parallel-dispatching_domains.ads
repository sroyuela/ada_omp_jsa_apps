with System.Multiprocessors.Dispatching_Domains;
with Ada.Task_Identification; use Ada;

package Parallel.Dispatching_Domains is

   type CPU_Set is array (CPU range <>) of Boolean;

   function Create
     (Set : CPU_Set)
      return System.Multiprocessors.Dispatching_Domains.Dispatching_Domain;

   function Get_CPU_Set
     (Domain : System.Multiprocessors.Dispatching_Domains.Dispatching_Domain)
      return CPU_Set;

   procedure Set_CPU
     (Set : CPU_Set;
      T   : Task_Identification.Task_Id :=
              Task_Identification.Current_Task);

end Parallel.Dispatching_Domains;
