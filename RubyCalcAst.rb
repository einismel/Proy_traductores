class TestNumNode
     def initialize(num,sign)
                @num = num
                @sign = sign
     end
end

class TestBinaryOpNode
        def initialize(term1, op, term2)
                @term1 = term1
                @op = op
                @term2 = term2
     end
end

class TestExpNode
     def initialize(exp)
                @exp = exp
     end
end 
