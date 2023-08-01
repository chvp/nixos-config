python-mode

(class "class " p ":"
       n> "\"\"\""
       n> p
       n> "\"\"\""
       n> "def __init__(self" (p ", *args, **kwargs") "):"
       n> "\"\"\""
       n> p
       n> "\"\"\""
       n> q
       )

(def "def " p "(" (p "*args, **kwargs") "):" n> "\"\"\"" n> p n> "\"\"\"" n> q)
