open System
open System.IO
open Newtonsoft.Json

type status =
    | Pending = 1
    | InProgress = 2
    | Completed = 3
    | Overdue = 4


type Task = {
    TaskID: int
    Description: string
    DueDate: DateOnly
    Priority: int
    Status: status
    CreationDate: DateOnly
}


let rec findMaxTaskID tasks currentMax =
    match tasks with
    | [] -> currentMax
    | task :: tail ->
        let newMax = if task.TaskID > currentMax then task.TaskID else currentMax
        findMaxTaskID tail newMax


let createTask tasks description dueDate priority status = 
    let newId = 
        if tasks = [] then 1
        else findMaxTaskID tasks 0 + 1
    let creationDate = DateOnly.FromDateTime(DateTime.Now)
    { TaskID = newId; Description = description; DueDate = dueDate; Priority = priority; Status = status; CreationDate = creationDate }



let updateTaskStatus task newStatus = 
    { task with Status = newStatus }


let updateTaskPriority task newPriority = 
    { task with Priority = newPriority }


let rec deleteTask tasks taskId =
    match tasks with
    | [] -> [] 
    | task :: tail ->
        if task.TaskID = taskId then
            deleteTask tail taskId 
        else
            task :: deleteTask tail taskId

let rec quickSortByDueDate tasks =
    match tasks with
    | [] -> [] 
    | pivot :: tail ->
        let smallerOrEqual = tail |> List.filter (fun task -> task.DueDate <= pivot.DueDate)
        let larger = tail |> List.filter (fun task -> task.DueDate > pivot.DueDate)
        quickSortByDueDate smallerOrEqual @ [pivot] @ quickSortByDueDate larger 

let sortTasksByDueDate tasks = 
    quickSortByDueDate tasks


let rec quickSortByPriority tasks =
    match tasks with
    | [] -> [] 
    | pivot :: tail ->
        let smallerOrEqual = tail |> List.filter (fun task -> task.Priority <= pivot.Priority)
        let larger = tail |> List.filter (fun task -> task.Priority > pivot.Priority)
        quickSortByPriority smallerOrEqual @ [pivot] @ quickSortByPriority larger 

let sortTasksByPriority tasks = 
    quickSortByPriority tasks


let highlightNearingDeadlines tasks daysBefore =
    let today = DateOnly.FromDateTime(DateTime.Now)
    
    let rec filterTasks remainingTasks highlightedTasks =
        match remainingTasks with
        | [] -> List.rev highlightedTasks 
        | task :: tail ->
            let dueSoon = task.DueDate >= today && task.DueDate <= today.AddDays(daysBefore)
            if dueSoon && (task.Status = status.Pending || task.Status = status.InProgress) then
                filterTasks tail (task :: highlightedTasks)
            else 
                filterTasks tail highlightedTasks 

    filterTasks tasks []


let notifyOverdueTasks tasks =
    let now = DateOnly.FromDateTime(DateTime.Now)
    
    let rec filterTasks remainingTasks overdueTasks =
        match remainingTasks with
        | [] -> List.rev overdueTasks 
        | task :: tail ->
            if (task.DueDate < now && (task.Status = status.Pending || task.Status = status.InProgress || task.Status = status.Overdue)) then
                filterTasks tail (task :: overdueTasks) 
            else
                filterTasks tail overdueTasks 

    filterTasks tasks []


let saveTasksToFile tasks filePath = 
    let json = JsonConvert.SerializeObject(tasks, Formatting.Indented)
    File.WriteAllText(filePath, json)


let loadTasksFromFile filePath = 
    let json = File.ReadAllText(filePath)
    JsonConvert.DeserializeObject<Task list>(json)


let displayTasks tasks =
    
    printfn "%-5s %-20s %-12s %-10s %-10s %-20s" "ID" "Description" "Due Date" "Priority" "Status" "Creation Date"
    printfn "%s" (String.replicate 80 "-") 

   
    let rec printTasks remainingTasks =
        match remainingTasks with
        | [] -> ()
        | task :: tail ->
            printfn "%-5d %-20s %-12s %-10d %-10s %-20s" 
                task.TaskID 
                task.Description 
                (task.DueDate.ToString("yyyy-MM-dd")) 
                task.Priority 
                (string task.Status)
                (task.CreationDate.ToString("yyyy-MM-dd"))
            printTasks tail 

    
    printTasks tasks

let rec tryFindTask tasks id =
    match tasks with
    | [] -> None 
    | head :: tail ->
        if head.TaskID = id then 
            Some head
        else 
            tryFindTask tail id

let rec promptForDate () =
    printf "Enter Due Date (yyyy-MM-dd): "
    let input = Console.ReadLine()
    match DateOnly.TryParseExact(input, "yyyy-MM-dd", null, Globalization.DateTimeStyles.None) with
    | (true, parsedDate) -> parsedDate 
    | (false, _) -> 
        printfn "Invalid date format. Please try again."
        promptForDate() 


let rec mainLoop tasks =
    printfn "\nChoose an option: "
    printfn "1. Add Task"
    printfn "2. Update Task"
    printfn "3. Delete Task"
    printfn "4. Display Tasks"
    printfn "5. Sort Tasks by Due Date"
    printfn "6. Sort Tasks by Priority"
    printfn "7. Highlight Tasks Nearing Deadlines"
    printfn "8. Notify Overdue Tasks"
    printfn "9. Save and Exit"
    let choice = Console.ReadLine()
    match choice with
    | "1" ->
        printf "Enter Description: "
        let description = Console.ReadLine()

        let dueDate = promptForDate()

        printf "Enter Priority: "
        let priority = int (Console.ReadLine())

        printfn "Choose Status: "
        printfn "1) Pending"
        printfn "2) In Progress"
        printfn "3) Completed"
        printfn "4) Overdue"
        let status = enum (int (Console.ReadLine()))

        let newTask = createTask tasks description dueDate priority status
        mainLoop (newTask :: tasks)
    | "2" ->
        printf "Enter Task ID to update: "
        let id = int (Console.ReadLine())
        
        match tryFindTask tasks id with
        | Some taskToUpdate ->
            printf "Enter New Priority: "
            let newPriority = int (Console.ReadLine())

            printfn "Choose new Status: "
            printfn "1) Pending"
            printfn "2) In Progress"
            printfn "3) Completed"
            printfn "4) Overdue"
            let newStatus = enum (int (Console.ReadLine())) 

            let updatedTaskv1 = updateTaskStatus taskToUpdate newStatus
            let updatedTaskv2 = updateTaskPriority updatedTaskv1 newPriority 

            let updatedTasks = updatedTaskv2 :: deleteTask tasks id

            mainLoop updatedTasks

        | None -> 
            printfn "Task with ID %d not found. Please try again." id
            mainLoop tasks
    | "3" ->
        printf "Enter Task ID to delete: "
        let id = int (Console.ReadLine())
        let updatedTasks = deleteTask tasks id
        mainLoop updatedTasks
    | "4" ->
        displayTasks tasks
        mainLoop tasks
    | "5" ->
        let sortTasksByDueDate = sortTasksByDueDate tasks
        displayTasks sortTasksByDueDate
        mainLoop sortTasksByDueDate
    | "6" ->
        let sortTasksByPriority = sortTasksByPriority tasks
        displayTasks sortTasksByPriority
        mainLoop sortTasksByPriority
    | "7" ->
        printf "Enter number of days before deadline to highlight: "
        let daysBefore = int (Console.ReadLine())
        let nearingDeadlines = highlightNearingDeadlines tasks daysBefore
        printfn "Tasks nearing deadlines:"
        displayTasks nearingDeadlines
        mainLoop tasks
    | "8" ->
        let overdueTasks = notifyOverdueTasks tasks
        printfn "Overdue Tasks:"
        displayTasks overdueTasks
        mainLoop tasks
    | "9" ->
        saveTasksToFile tasks "tasks.json"
        printfn "Tasks saved. Exiting..."
    | _ ->
        printfn "Invalid choice. Try again."
        mainLoop tasks


let tasks = 
    if File.Exists("tasks.json") then
        loadTasksFromFile "tasks.json"
    else
        []


mainLoop tasks