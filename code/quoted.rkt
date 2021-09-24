
(define/match (num-plus datum)
    [((list expr ...)) (apply + (map num-plus expr))]
    [('+) 1]
    [(_) 0])