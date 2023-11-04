(loop for i = 1234 then (floor i 10)
        while (> i 0)
        collect (mod i 10))