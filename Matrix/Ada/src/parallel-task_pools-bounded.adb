package body Parallel.Task_Pools.Bounded is

   protected body Task_Manager is

      function Active_Workers return Worker_Count_Type is
      begin
         return Outstanding_Workers;
      end Active_Workers;

      entry Allocate_Workers
        (Worker_Count : Positive_Worker_Count;
         First_Worker : out Worker_Id)
        when Outstanding_Workers + Requested_Workers <= Number_Of_Workers is
      begin
         --  Get the index of the first worker of a block of workers in the
         --  Idle worker list
         First_Worker :=
           Number_Of_Workers - (Outstanding_Workers + Worker_Count);
         Busy := True;
      end Allocate_Workers;

      procedure Assign
        (Domain : in out Dispatching_Domains.Dispatching_Domain) is
      begin
         for I in Workers'Range loop
            Dispatching_Domains.Assign_Task
              (Domain => Domain,
               CPU    => Multiprocessors.Not_A_Specific_CPU,
               T      => Workers (I)'Identity);
         end loop;
      end Assign;

      entry Completed_Work (Worker : Worker_Id) when not Busy is
      begin
         Outstanding_Workers := Outstanding_Workers - 1;
         Idle_Workers (Number_Of_Workers - Outstanding_Workers) := Worker;
      end Completed_Work;

      procedure Finished_Offer is
      begin
         Busy := False;
      end Finished_Offer;

      entry Offer_Work (Item : aliased in out Work_Plan'Class;
                        Priority : System.Priority)
        --  when Outstanding_Workers < Number_Of_Workers
        when True
      is
         Worker_Index : Positive_Worker_Count;
      begin

         Worker_Index := Idle_Workers
              (Number_Of_Workers - Outstanding_Workers);
         Outstanding_Workers := Outstanding_Workers + 1;
         requeue Workers (Worker_Index).Work_Offered;

      end Offer_Work;

      entry Request_Workers
        (Worker_Count : Positive_Worker_Count;
         First_Worker : out Worker_Id) when True is
      begin
         Requested_Workers := Worker_Count;
         requeue Allocate_Workers;
      end Request_Workers;

   end Task_Manager;

   task body Worker is
      Plan : access Work_Plan'Class;
   begin -- Worker

      Work_Loop : loop
         begin

            select

               accept Work_Offered (Item : aliased in out Work_Plan'Class;
                                    Priority : System.Priority)
               do
                  Dynamic_Priorities.Set_Priority (Priority);
                  Plan := Item'Unchecked_Access;
               end Work_Offered;

            or
               terminate;

            end select;

            Plan.Engage;

            Pool.Manager.Completed_Work (Id);

         exception
            when others =>
               --  Should probably at least log something here,
               --  but we dont want exceptions to take tasks out of
               --  the task pool
               null;
         end;
      end loop Work_Loop;

   end Worker;

   --------------------------------------------------

   function Create
     (Number_Of_Workers : Positive_Worker_Count;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size;
      Ceiling_Priority : System.Priority :=
        System.Max_Priority)
      return Task_Pool
   is
   begin
      return Pool : Task_Pool
        (Number_Of_Workers,
         Storage_Size,
         Ceiling_Priority)
      do
         null;
      end return;

   end Create;

   --------------------------------------------------

   function Create
     (Number_Of_Workers : Positive_Worker_Count;
      Storage_Size : System.Storage_Elements.Storage_Count :=
        Default_Worker_Storage_Size;
      Ceiling_Priority : System.Priority :=
        System.Max_Priority;
      Domain : in out Dispatching_Domains.Dispatching_Domain)
      return Task_Pool is
   begin
      return Pool : Task_Pool
        (Number_Of_Workers,
         Storage_Size,
         Ceiling_Priority)
      do

         --  Assign all the workers in the pool to the specific domain
         Pool.Manager.Assign (Domain);

      end return;
   end Create;

   --------------------------------------------------

   function Create_Idle_List
     (Number_Of_Workers : Positive_Worker_Count) return Idle_List is
   begin
      return Result : Idle_List (1 .. Number_Of_Workers) do
         for I in Result'Range loop
            Result (I) := I;
         end loop;
      end return;
   end Create_Idle_List;

   --------------------------------------------------

   function Create_Worker
     (Pool : access Task_Pool'Class;
      Storage_Size : System.Storage_Elements.Storage_Count) return Worker
   is
      Id : constant Worker_Id := Pool.Next_Id;
   begin
      Pool.Next_Id := Pool.Next_Id + 1;

      return New_Worker : Worker (Pool,
                                  Id,
                                  Storage_Size)
      do
         null;
      end return;
   end Create_Worker;

   --------------------------------------------------

   procedure Offer_Work
     (Pool : in out Task_Pool;
      Item : aliased in out Work_Plan'Class;
      Worker_Count : Positive_Worker_Count)
   is
      Current_Priority : constant System.Priority
        := Dynamic_Priorities.Get_Priority;
      First_Worker : Worker_Id;
   begin
      Pool.Manager.Request_Workers (Worker_Count, First_Worker);

      for I in 1 .. Worker_Count loop
         Pool.Manager.Offer_Work (Item, Current_Priority);
      end loop;

      Pool.Manager.Finished_Offer;
   end Offer_Work;

end Parallel.Task_Pools.Bounded;
