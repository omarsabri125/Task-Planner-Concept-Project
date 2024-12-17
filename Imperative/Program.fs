open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json

type Task = {
    Id: string
    Description: string
    DueDate: DateTime
    Priority: int
    Status: string
    CreatedAt: DateTime
}

let mutable tasks: Task list = []

let generateTaskId (description: string) (dueDate: DateTime) (createdAt: DateTime) =
    let inputString = sprintf "%s|%s|%s" (createdAt.ToString("o")) (dueDate.ToString("o")) description
    use sha256 = SHA256.Create()
    let hashBytes = sha256.ComputeHash(Encoding.UTF8.GetBytes(inputString))
    BitConverter.ToString(hashBytes).Replace("-", "").Substring(0, 6)

let rec getValidInt prompt =
    printf "%s" prompt
    match Int32.TryParse(Console.ReadLine()) with
    | true, value -> value
    | _ -> 
        printfn "Invalid input. Please enter a valid number."
        getValidInt prompt

let rec getValidDate prompt =
    printf "%s" prompt
    match DateTime.TryParse(Console.ReadLine()) with
    | true, value when value > DateTime.Now -> value 
    | true, _ -> 
        printfn "The due date must be in the future. Please try again."
        getValidDate prompt
    | _ -> 
        printfn "Invalid input. Please enter a valid date and time (e.g., YYYY-MM-DD HH:MM)."
        getValidDate prompt

let bubbleSort (list: 'a list) (compare: 'a -> 'a -> int) =
    let arr = list |> List.toArray
    let len = Array.length arr
    for i in 0 .. len - 2 do
        for j in 0 .. len - i - 2 do
            if compare arr[j] arr[j + 1] > 0 then
                let temp = arr[j]
                arr[j] <- arr[j + 1]
                arr[j + 1] <- temp
    arr |> Array.toList

let highlightDeadlines () =
    let today = DateTime.Now

    let mutable tasksNearDeadline = []
    let mutable overdueTasks = []

    for t in tasks do
        if t.Status = "Pending" then
            let daysRemaining = (t.DueDate - today).TotalDays

            if daysRemaining <= 3.0 && daysRemaining > 0.0 then
                tasksNearDeadline <- t :: tasksNearDeadline

            if t.DueDate < today then
                overdueTasks <- t :: overdueTasks

    let mutable nearDeadlineString = ""
    if List.length tasksNearDeadline = 0 then
        nearDeadlineString <- "[+] No tasks near deadlines.\n"
    else
        nearDeadlineString <- "[+] Tasks Near Deadlines (Due in 3 Days or Less):\n"
        for t in tasksNearDeadline do
            nearDeadlineString <- nearDeadlineString + sprintf "- ID: %s, %s (Due: %s)\n" t.Id t.Description (t.DueDate.ToString("yyyy-MM-dd - HH:mm"))

    let mutable overdueString = ""
    if List.length overdueTasks = 0 then
        overdueString <- "[+] No overdue tasks.\n"
    else
        overdueString <- "[+] Overdue Tasks:\n"
        for t in overdueTasks do
            overdueString <- overdueString + sprintf "- ID: %s, %s (Due: %s)\n" t.Id t.Description (t.DueDate.ToString("yyyy-MM-dd - HH:mm"))

    nearDeadlineString + overdueString


let addTask () =
    printfn "Add New Task:\nDescription:"
    let description = Console.ReadLine()
    let dueDate = getValidDate "Due Date and Time (YYYY-MM-DD HH:MM): "
    let priority = getValidInt "Priority (1 = High, 2 = Medium, 3 = Low): "
    let status = "Pending"
    let createdAt = DateTime.Now 
    let id = generateTaskId description dueDate createdAt
    let newTask = { Id = id; Description = description; DueDate = dueDate; Priority = priority; Status = status; CreatedAt = createdAt }
    tasks <- tasks @ [newTask]
    printfn "Task added successfully."

let updateTask () =
    if tasks = [] then
        printfn "No tasks available to update. Press Enter to return to the main menu."
        Console.ReadLine() |> ignore
    else
        printf "Enter Task ID to update: "
        let id = Console.ReadLine()

        let mutable taskFound = false
        let mutable updatedTasks = [] 

        for t in tasks do
            if t.Id = id then
                taskFound <- true
                printfn "1. Mark as Completed"
                printfn "2. Update Priority"

                match Console.ReadLine() with
                | "1" ->
                    updatedTasks <- { t with Status = "Completed" } :: updatedTasks
                    printfn "Task marked as completed."
                | "2" ->
                    let newPriority = getValidInt "Enter new priority (1 = High, 2 = Medium, 3 = Low): "
                    updatedTasks <- { t with Priority = newPriority } :: updatedTasks
                    printfn "Task priority updated."
                | _ ->
                    updatedTasks <- t :: updatedTasks
                    printfn "Invalid choice."
            else
                updatedTasks <- t :: updatedTasks

        if not taskFound then
            printfn "Task not found."

let deleteTask () =
    if tasks = [] then
        printfn "No tasks available to delete. Press Enter to return to the main menu."
        Console.ReadLine() |> ignore
    else
        printf "Enter Task ID to delete: "
        let id = Console.ReadLine()

        let mutable taskFound = false
        let mutable newTasks = []

        for t in tasks do
            if t.Id <> id then
                newTasks <- newTasks @ [t]  
            else
                taskFound <- true  

        if taskFound then
            tasks <- newTasks  
            printfn "Task deleted successfully."
        else
            printfn "Task not found."

let filterTasks () =
    if tasks = [] then
        printfn "No tasks available to filter. Press Enter to return to the main menu."
        Console.ReadLine() |> ignore
    else
        printfn "Filter Tasks by:"
        printfn "1. Status"
        printfn "2. Priority"

        let choice = Console.ReadLine()
        match choice with
        | "1" ->
            printf "Enter status (1 = Pending, 2 = Completed): "
            let statusChoice = Console.ReadLine()
            let mutable filteredTasks = [] 

            if statusChoice = "1" then
                for t in tasks do
                    if t.Status = "Pending" then
                        filteredTasks <- t :: filteredTasks
            elif statusChoice = "2" then
                for t in tasks do
                    if t.Status = "Completed" then
                        filteredTasks <- t :: filteredTasks
            else
                printfn "Invalid choice."
            for t in filteredTasks do
                printfn "%A" t

        | "2" ->
            let priority = getValidInt "Enter priority (1 = High, 2 = Medium, 3 = Low): "
            let mutable filteredTasks = [] 

            for t in tasks do
                if t.Priority = priority then
                    filteredTasks <- t :: filteredTasks

            for t in filteredTasks do
                printfn "%A" t

        | _ -> printfn "Invalid choice."

let sortTasks () =
    if tasks = [] then
        printfn "No tasks available to sort. Press Enter to return to the main menu."
        Console.ReadLine() |> ignore
    else
        printfn "Sort Tasks by:"
        printfn "1. Due Date"
        printfn "2. Priority"
        printfn "3. Creation Time"

        let choice = Console.ReadLine()
        match choice with
        | "1" ->
            let sortedTasks = bubbleSort tasks (fun t1 t2 -> compare t1.DueDate t2.DueDate)
            for task in sortedTasks do
                printfn "%A" task
        | "2" ->
            let sortedTasks = bubbleSort tasks (fun t1 t2 -> compare t1.Priority t2.Priority)
            for task in sortedTasks do
                printfn "%A" task
        | "3" ->
            let sortedTasks = bubbleSort tasks (fun t1 t2 -> compare t1.CreatedAt t2.CreatedAt)
            for task in sortedTasks do
                printfn "%A" task
        | _ ->
            printfn "Invalid choice."

let saveTasksToFile () =
    printf "Enter file name to save tasks: "
    let fileName = Console.ReadLine()
    let json = JsonSerializer.Serialize(tasks)
    File.WriteAllText(fileName, json)
    printfn "Tasks saved successfully."

let loadTasksFromFile () =
    printf "Enter file name to load tasks: "
    let fileName = Console.ReadLine()
    if File.Exists(fileName) then
        let json = File.ReadAllText(fileName)
        tasks <- JsonSerializer.Deserialize<Task list>(json)
        printfn "Tasks loaded successfully."
    else
        printfn "File not found."

let rec mainMenu () =

    let highlightedTasks = highlightDeadlines()
    printfn "\n=== Highlighted Tasks ==="
    printfn "%s" highlightedTasks

    printfn "Task Scheduler"
    printfn "1. Add Task"
    printfn "2. Update Task"
    printfn "3. Delete Task"
    printfn "4. Filter Tasks"
    printfn "5. Sort Tasks"
    printfn "6. Save Tasks to File"
    printfn "7. Load Tasks from File"
    printfn "0. Exit"
    match Console.ReadLine() with
    | "1" -> addTask(); mainMenu ()
    | "2" -> updateTask(); mainMenu ()
    | "3" -> deleteTask(); mainMenu ()
    | "4" -> filterTasks(); mainMenu ()
    | "5" -> sortTasks(); mainMenu ()
    | "6" -> saveTasksToFile(); mainMenu ()
    | "7" -> loadTasksFromFile(); mainMenu ()
    | "0" -> printfn "Exiting..."; exit 0
    | _ -> printfn "Invalid choice."; mainMenu ()

[<EntryPoint>]
let main argv =
    mainMenu ()
    0
