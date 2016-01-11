module File1

    let incr =
        let mutable counter = 0
        counter <- counter + 1
        counter

    let incr1 =
        let counter = ref 0
        fun () ->
            counter := !counter + 1
            !counter;;

    let caller =
        let counter = incr
        counter