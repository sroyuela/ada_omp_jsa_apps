package body Parallel.Dispatching_Domains is

   ------------
   -- Create --
   ------------

   function Create
     (Set : CPU_Set)
      return System.Multiprocessors.Dispatching_Domains.Dispatching_Domain is
   begin
      return X : constant
        System.Multiprocessors.Dispatching_Domains.Dispatching_Domain
          := System.Multiprocessors.Dispatching_Domains.Create
            (First => Set'First,
             Last  => Set'Last)
      do
         null;
      end return;

   end Create;

   ----------------------------------------------------------------

   function Get_CPU_Set
     (Domain : System.Multiprocessors.Dispatching_Domains.Dispatching_Domain)
      return CPU_Set
   is
      Result : constant CPU_Set
        (System.Multiprocessors.Dispatching_Domains.Get_First_CPU (Domain) ..
         System.Multiprocessors.Dispatching_Domains.Get_Last_CPU (Domain))
        := (others => True);
   begin
      return Result;
   end Get_CPU_Set;

   ----------------------------------------------------------------

   procedure Set_CPU
     (Set : CPU_Set;
      T   : Task_Identification.Task_Id :=
        Task_Identification.Current_Task) is
      pragma Unreferenced (Set, T);
   begin
      System.Multiprocessors.Dispatching_Domains.Set_CPU (1);
   end Set_CPU;

end Parallel.Dispatching_Domains;
